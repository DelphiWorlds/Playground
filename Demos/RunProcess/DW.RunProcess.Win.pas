unit DW.RunProcess.Win;

interface

uses
  // RTL
  System.Classes, System.SysUtils, System.SyncObjs,
  // Windows
  Winapi.Windows,
  // DW
  DW.RunProcess;

type
  TIOHandles = record
    Error: THandle;
    Read: THandle;
    Write: THandle;
  private
    procedure Close;
  end;

  TCustomRunProcess = class;

  TProcess = class(TObject)
  private
    FCanTerminate: Boolean;
    FCommandLine: string;
    FConsoleHandles: TIOHandles;
    FExitCode: Cardinal;
    FIsTerminated: Boolean;
    FLocalHandles: TIOHandles;
    FProcessInfo: TProcessInformation;
    FRunProcess: TCustomRunProcess;
    procedure CloseHandles;
    procedure CloseIOHandles;
    procedure CloseProcessHandles;
    procedure CloseSuperfluousHandles;
    procedure ConstructPipes;
    procedure DoStarted;
    procedure DoOutput(const AOutput: TBytes; const ASize: Cardinal);
    procedure DoTerminated;
    procedure HandleException(const AException: Exception);
    function InternalRun(const ATimeout: Cardinal): Integer;
    procedure Queue(const AMethod: TThreadProcedure);
    procedure UpdateExitCode;
    function WaitOnProcess(const ATimeout: Cardinal): Boolean;
  public
    class procedure Terminate(const AProcessHandle: THandle);
  public
    constructor Create(const ARunProcess: TCustomRunProcess);
    function IsRunning: Boolean;
    function StartProcess: Boolean;
    function Run(const ATimeout: Cardinal): Integer;
  end;

  TReadThread = class(TThread)
  private
    FEvent: TEvent;
    FInputBuffer: TBytes;
    FInputBufferPos: Cardinal;
    FReadBuffer: TBytes;
    FReadHandle: THandle;
    FReadLock: TCriticalSection;
    procedure CopyReadBuffer(const ASize: Cardinal);
  protected
    procedure Execute; override;
  public
    constructor Create(const AReadHandle: THandle);
    destructor Destroy; override;
    function ReadBuffer(var ABuffer: TBytes; out ASize: Cardinal): Boolean;
    property Event: TEvent read FEvent;
  end;

  /// <summary>
  ///   Custom class for running Windows processes
  /// </summary>
  /// <remarks>
  ///   Derive from this class when event handlers are not required
  /// </remarks>
  TCustomRunProcess = class(TBaseRunProcess)
  private
    FExitCode: Cardinal;
    FIsRunning: Boolean;
    FProcessHandle: THandle;
  protected
    FCommandLine: string;
    procedure BeforeRun;
    procedure DoOutput(const AOutput: string); override;
    procedure DoRun;
    function GetIsRunning: Boolean; override;
    function InternalRun: Boolean;
    function MakeWorkingPath: string;
    procedure ProcessOutput(const AOutput: TBytes);
    procedure ProcessStarted(const AProcessHandle: THandle);
    procedure ProcessTerminated(const AExitCode: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;
    function RunAndWait(const ATimeout: Cardinal): Integer; override;
    procedure Terminate; override;
    property CommandLine: string read FCommandLine write FCommandLine;
  end;

  /// <summary>
  ///   Concrete class for running Windows processes
  /// </summary>
  TRunProcess = class(TCustomRunProcess)
  public
    function Run: Boolean; override;
    procedure WriteInput(const AInput: string; const AIncludeReturn: Boolean = True);
    property OnProcessOutput;
    property OnProcessPartialOutput;
    property OnProcessTerminated;
  end;

implementation

uses
  // RTL
  System.IOUtils, System.DateUtils, System.Math; // ,
  // DW
  // DW.OSLog;

const
  cReadBufferSize = 1024;

type
  TDuplicateKind = (KeepSourceOpen, CloseSource);

procedure SafeCloseHandle(const AHandle: THandle);
begin
  if AHandle > 0 then
  try
    CloseHandle(AHandle);
  except
    // Eat it, Harvey
  end;
end;

function OSCheck(const ARetVal: Boolean): Boolean;
begin
  if not ARetVal then
    RaiseLastOSError;
  Result := ARetVal;
end;

function CreateDuplicate(const AHandle: THandle; const AKind: TDuplicateKind): THandle;
const
  cCloseAction: array [TDuplicateKind] of DWORD = (0, DUPLICATE_CLOSE_SOURCE);
begin
  OSCheck(DuplicateHandle(GetCurrentProcess, AHandle,
    GetCurrentProcess,
    @Result, // Address of new handle.
    0, AKind = TDuplicateKind.KeepSourceOpen,
    DUPLICATE_SAME_ACCESS or cCloseAction[AKind]));
end;

{ TIOHandles }

procedure TIOHandles.Close;
begin
  if Error > 0 then
    SafeCloseHandle(Error);
  if Read > 0 then
    SafeCloseHandle(Read);
  if Write > 0 then
    SafeCloseHandle(Write);
  Error := 0;
  Read := 0;
  Write := 0;
end;

{ TReadThread }

constructor TReadThread.Create(const AReadHandle: THandle);
begin
  inherited Create(False);
  FReadLock := TCriticalSection.Create;
  FEvent := TEvent.Create(nil, False, False, '');
  SetLength(FInputBuffer, cReadBufferSize);
  SetLength(FReadBuffer, cReadBufferSize);
  FReadHandle := AReadHandle;
end;

destructor TReadThread.Destroy;
begin
  FEvent.Free;
  FReadLock.Free;
  inherited;
end;

procedure TReadThread.Execute;
var
  LBytesRead, LSize: Cardinal;
  LDebugSize: Cardinal;
begin
  LSize := Length(FReadBuffer);
  while not Terminated do
  begin
    if not ReadFile(FReadHandle, FReadBuffer, LSize, LBytesRead, nil) then
    begin
      if GetLastError = ERROR_BROKEN_PIPE then
        Break;
    end
    else
      CopyReadBuffer(LBytesRead);
  end;
end;

procedure TReadThread.CopyReadBuffer(const ASize: Cardinal);
var
  LInputBufferLen: Cardinal;
begin
  FReadLock.Acquire;
  try
    LInputBufferLen := Length(FInputBuffer);
    if FInputBufferPos + ASize > LInputBufferLen then
      SetLength(FInputBuffer, LInputBufferLen + cReadBufferSize);
    Move(FReadBuffer[0], FInputBuffer[FInputBufferPos], ASize);
    Inc(FInputBufferPos, ASize);
  finally
    FReadLock.Release;
  end;
  FEvent.SetEvent;
end;

function TReadThread.ReadBuffer(var ABuffer: TBytes; out ASize: Cardinal): Boolean;
begin
  Result := False;
  FReadLock.Acquire;
  try
    if FInputBufferPos > 0 then
    begin
      ASize := Min(FInputBufferPos, cReadBufferSize);
      Move(FInputBuffer[0], ABuffer[0], ASize);
      // If not all data in FInputBuffer is copied to ABuffer, then place
      // the data not copied at the begin of FInputBuffer.
      if FInputBufferPos > ASize then
        Move(FInputBuffer[ASize], FInputBuffer[0], FInputBufferPos - ASize);
      Dec(FInputBufferPos, ASize);
    end;
  finally
    FReadLock.Release;
  end;
end;

{ TProcess }

constructor TProcess.Create(const ARunProcess: TCustomRunProcess);
begin
  inherited Create;
  FRunProcess := ARunProcess;
  FCommandLine := FRunProcess.CommandLine;
end;

class procedure TProcess.Terminate(const AProcessHandle: THandle);
var
  LOpenHandle: THandle;
begin
  if AProcessHandle > 0 then
  begin
    LOpenHandle := OpenProcess(PROCESS_TERMINATE, False, AProcessHandle);
    if LOpenHandle <> 0 then
    try
      TerminateProcess(LOpenHandle, 0);
    finally
      CloseHandle(LOpenHandle);
    end;
  end;
end;

procedure TProcess.ConstructPipes;
var
  LSa: TSecurityAttributes;
  LSd: TSecurityDescriptor;
  LRead, LWrite: THandle;
begin
  FConsoleHandles.Close;
  FLocalHandles.Close;
  FillChar(LSa, SizeOf(TSecurityAttributes), 0);
  LSa.nLength := SizeOf(TSecurityAttributes);
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    InitializeSecurityDescriptor(@LSd, SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(@LSd, True, nil, False);
    LSa.lpSecurityDescriptor := @LSd;
  end
  else
    LSa.lpSecurityDescriptor := nil;
  LSa.bInheritHandle := True;
  // Create a pipe between StdOutput and a local reader
  OSCheck(CreatePipe(LRead, LWrite, @LSa, 0));
  FConsoleHandles.Write := LWrite;
  FLocalHandles.Read := CreateDuplicate(LRead, TDuplicateKind.CloseSource);
  // Create a pipe between StdError and a local reader - unused as yet
  OSCheck(CreatePipe(LRead, LWrite, @LSa, 0));
  FConsoleHandles.Error := LWrite;
  FLocalHandles.Error := CreateDuplicate(LRead, TDuplicateKind.CloseSource);
  // Create a pipe between StdInput and a local writer - unused as yet
  OSCheck(CreatePipe(LRead, LWrite, @LSa, 0));
  FConsoleHandles.Read := LRead;
  FLocalHandles.Write := CreateDuplicate(LWrite, TDuplicateKind.CloseSource);
end;

function TProcess.IsRunning: Boolean;
begin
  Result := FExitCode = STILL_ACTIVE;
end;

function TProcess.StartProcess: Boolean;
var
  LExecuting: string;
  LStartupInfo: TStartupInfo;
  LCommand: string;
begin
  ConstructPipes;
  ZeroMemory(@LStartupInfo, SizeOf(LStartupInfo));
  LStartupInfo.cb := SizeOf(LStartupInfo);
  LStartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  LStartupInfo.wShowWindow := SW_HIDE;
  LStartupInfo.hStdInput := FConsoleHandles.Read;
  LStartupInfo.hStdOutput := FConsoleHandles.Write;
  LStartupInfo.hStdError := FConsoleHandles.Error;
  LCommand := FCommandLine;
  UniqueString(LCommand);
  Result := CreateProcess(nil, PChar(LCommand), nil, nil, True, CREATE_NO_WINDOW or NORMAL_PRIORITY_CLASS, nil, nil, LStartupInfo, FProcessInfo);
  if Result then
  begin
    UpdateExitCode;
    CloseSuperfluousHandles;
  end
  else
    CloseHandles;
end;

procedure TProcess.CloseHandles;
begin
  CloseProcessHandles;
  CloseIOHandles;
end;

procedure TProcess.CloseProcessHandles;
begin
  if FProcessInfo.hProcess > 0 then
    SafeCloseHandle(FProcessInfo.hProcess);
  if FProcessInfo.hThread > 0 then
    SafeCloseHandle(FProcessInfo.hThread);
  ZeroMemory(@FProcessInfo, SizeOf(FProcessInfo));
end;

procedure TProcess.CloseSuperfluousHandles;
begin
  FConsoleHandles.Close;
end;

procedure TProcess.CloseIOHandles;
begin
  FConsoleHandles.Close;
  FLocalHandles.Close;
end;

procedure TProcess.UpdateExitCode;
begin
  if FProcessInfo.hProcess > 0 then
    GetExitCodeProcess(FProcessInfo.hProcess, FExitCode);
end;

function TProcess.WaitOnProcess(const ATimeout: Cardinal): Boolean;
var
  LWaitHandles: array[0..1] of THandle; // 0..2
  LReadOutputThread: TReadThread;
  LReadBuffer: TBytes;
  LReadSize: Cardinal;
begin
  Result := True;
  SetLength(LReadBuffer, cReadBufferSize);
  LReadOutputThread := TReadThread.Create(FLocalHandles.Read);
  try
    LWaitHandles[0] := FProcessInfo.hProcess;
    LWaitHandles[1] := LReadOutputThread.Event.Handle;
//  LWaitHandles[2] := FInputLines.Event.Handle;
    repeat
      case WaitforMultipleObjects(Length(LWaitHandles), @LWaitHandles, False, ATimeout) of
        // Process terminated
        WAIT_OBJECT_0 + 0:
          UpdateExitCode;
        // ReadEvent
        WAIT_OBJECT_0 + 1:
        begin
          while LReadOutputThread.ReadBuffer(LReadBuffer, LReadSize) do
            DoOutput(LReadBuffer, LReadSize);
        end;
        // InputEvent
        WAIT_OBJECT_0 + 2:
        begin
//          if ((FInputLines.Count > 0) and not(Terminated)) then
//            DoSendLine(inputWrite, last, LineBeginned);
//          if FInputLines.Count > 0 then
//            FInputLines.Event.SetEvent;
        end;
        WAIT_TIMEOUT:
          Result := False;
      end;
    until not IsRunning or not Result;
    UpdateExitCode; // <--- not sure if this will be necessary
    if not Result and IsRunning then
    begin
      Terminate(FProcessInfo.hProcess);
      WaitForSingleObject(FProcessInfo.hProcess, 1000);
      UpdateExitCode;
    end;
  finally
    LReadOutputThread.Free;
  end;
end;

procedure TProcess.Queue(const AMethod: TThreadProcedure);
begin
  TThread.Queue(nil, AMethod);
end;

procedure TProcess.DoOutput(const AOutput: TBytes; const ASize: Cardinal);
var
  LOutput: TBytes;
begin
  SetLength(LOutput, ASize);
  Move(AOutput[0], LOutput[0], ASize);
  Queue(
    procedure
    begin
      FRunProcess.ProcessOutput(LOutput);
    end
  );
end;

procedure TProcess.DoStarted;
begin
  Queue(
    procedure
    begin
      FRunProcess.ProcessStarted(FProcessInfo.hProcess);
    end
  );
end;

procedure TProcess.DoTerminated;
var
  LExitCode: Cardinal;
begin
  LExitCode := FExitCode;
  Queue(
    procedure
    begin
      FRunProcess.ProcessTerminated(LExitCode);
    end
  );
end;

function TProcess.InternalRun(const ATimeout: Cardinal): Integer;
begin
  Result := -1;
  if StartProcess then
  try
    DoStarted;
    if WaitOnProcess(ATimeout) then
      Result := 0
    else
      Result := 1;
  finally
    CloseHandles;
  end;
end;

function TProcess.Run(const ATimeout: Cardinal): Integer;
begin
  Result := -1;
  try
    Result := InternalRun(ATimeout);
  except
    on E: Exception do
      HandleException(E); // re-raise?
  end;
end;

procedure TProcess.HandleException(const AException: Exception);
var
  LDebugMessage: string;
begin
  LDebugMessage := Format('%s: %s', [AException.ClassName, AException.Message]);
  Sleep(0);
end;

{ TCustomRunProcess }

constructor TCustomRunProcess.Create;
begin
  inherited;
  //
end;

destructor TCustomRunProcess.Destroy;
begin
  // Needs an Event to signal the anon thread
  Terminate;
  inherited;
end;

procedure TCustomRunProcess.DoOutput(const AOutput: string);
begin
  FCapturedOutput := FCapturedOutput + [AOutput];
  inherited;
end;

procedure TCustomRunProcess.ProcessOutput(const AOutput: TBytes);
begin
  DoOutput(TEncoding.UTF8.GetString(AOutput));
end;

procedure TCustomRunProcess.ProcessStarted(const AProcessHandle: THandle);
begin
  FIsRunning := True;
  FProcessHandle := AProcessHandle;
end;

procedure TCustomRunProcess.ProcessTerminated(const AExitCode: Cardinal);
begin
  FIsRunning := False;
  FProcessHandle := 0;
  FExitCode := ExitCode;
  DoTerminated(ExitCode);
end;

procedure TCustomRunProcess.BeforeRun;
var
  LExecuting: string;
begin
  FCapturedOutput := [];
  LExecuting := 'Executing: ' + FCommandLine;
  if ShowExecuting then
    DoOutput(LExecuting);
//  if ShowCommandInLog then
//    TOSLog.d(LExecuting);
end;

procedure TCustomRunProcess.DoRun;
var
  LProcess: TProcess;
begin
  LProcess := TProcess.Create(Self);
  try
    LProcess.Run(INFINITE);
  finally
    LProcess.Free;
  end;
end;

function TCustomRunProcess.InternalRun: Boolean;
begin
  BeforeRun;
  TThread.CreateAnonymousThread(DoRun).Start;
  Result := True;
end;

function TCustomRunProcess.RunAndWait(const ATimeout: Cardinal): Integer;
var
  LProcess: TProcess;
begin
  BeforeRun;
  LProcess := TProcess.Create(Self);
  try
    Result := LProcess.Run(ATimeout);
  finally
    LProcess.Free;
  end;
end;

procedure TCustomRunProcess.Terminate;
begin
  TProcess.Terminate(FProcessHandle);
end;

{
function TCustomRunProcess.WaitForProcess(const ATimeout: Cardinal): Boolean;
begin
  try
    Result := WaitForSingleObject(FProcessInfo.hProcess, ATimeout) <> WAIT_TIMEOUT;
    if Result then
      UpdateExitCode;
  finally
    CloseHandles;
  end;
end;
}

function TCustomRunProcess.GetIsRunning: Boolean;
begin
  Result := False; //!!!!!
end;

function TCustomRunProcess.MakeWorkingPath: string;
var
  LGUID: string;
begin
  LGUID := TGUID.NewGuid.ToString;
  Result := TPath.Combine(TPath.GetTempPath, LGUID.Substring(1, LGUID.Length - 2));
  ForceDirectories(Result);
//  if not TDirectory.Exists(Result) then
//    TOSLog.d('Unable to create working path: %s', [Result]);
end;

{ TRunProcess }

function TRunProcess.Run: Boolean;
begin
  Result := InternalRun;
end;

procedure TRunProcess.WriteInput(const AInput: string; const AIncludeReturn: Boolean = True);
begin
//  if AIncludeReturn then
//    FProcess.WriteLn(AInput)
//  else
//    FProcess.Write(AInput);
end;

end.
