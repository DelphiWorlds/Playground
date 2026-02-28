unit DW.GrijjyErrorReporting;

// **NOTE**: This unit is dependent on files from:
//   https://github.com/grijjy/JustAddCode/tree/master/ErrorReporting
//
// Clone or download the JustAddCode repo, and modify your project search path to include the ErrorReporting folder
// As at Feb 28th, 2026, Windows is *NOT* supported by this code
//
// As this code also references Kastri:
//   https://github.com/DelphiWorlds/Kastri
// You will also need to clone or download that repo, and ensure the project search path refers to the API and Core folders of Kastri
//
// For Android:
//   1. Copy the goExports.vsr file into the project folder
//   2. In the Building > Delphi Compiler > Linker Options section of the project options, add to the Options passed to the LD linker setting:
//
//      --version-script=goExports.vsr
//
//   3. If including this unit in a service associated with an Android app, in the Building > Delphi Compiler section of the project options,
//      add to the Conditional Defines setting:
//
//      SERVICE
//
//      ..of the *service* project
//
//  For how to view the log messages emitted, see:
//    https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/LogViewers#viewing-the-logs

interface

uses
  Grijjy.ErrorReporting;

type
  IgoExceptionReport = Grijjy.ErrorReporting.IgoExceptionReport;

  TExceptionReportProc = reference to procedure(const Report: IgoExceptionReport);

  IExceptionInterceptor = interface
    ['{9CD6529F-4000-42BD-84C3-DF4337B9D35A}']
    // Future: Add methods/properties to set options
    procedure AddHandler(const AHandler: TExceptionReportProc);
  end;

var
  ExceptionInterceptor: IExceptionInterceptor;

implementation

uses
  System.Classes, System.Messaging, System.SysUtils,
  {$IF not Defined(SERVICE)}
  FMX.Forms,
  {$ENDIF}
  DW.OSLog;

type
  TExceptionReportProcs = TArray<TExceptionReportProc>;

  TExceptionInterceptor = class(TInterfacedObject, IExceptionInterceptor)
  private
    FHandlers: TExceptionReportProcs;
    procedure ExceptionReportMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure LogCallstack(const ACallstack: TgoCallStack);
  public
    { IExceptionInterceptor }
    procedure AddHandler(const AHandler: TExceptionReportProc);
  public
    constructor Create;
    destructor Destroy; override;
  end;

function AddressToString(const AAddress: UIntPtr): String;
begin
  {$IFDEF CPU64BITS}
  Result := '$' + IntToHex(AAddress, 16);
  {$ELSE}
  Result := '$' + IntToHex(AAddress, 8);
  {$ENDIF}
end;

{ TExceptionInterceptor }

constructor TExceptionInterceptor.Create;
begin
  inherited;
  {$IF not Defined(SERVICE)}
  Application.OnException := TgoExceptionReporter.ExceptionHandler;
  {$ENDIF}
  TMessageManager.DefaultManager.SubscribeToMessage(TgoExceptionReportMessage, ExceptionReportMessageHandler);
end;

destructor TExceptionInterceptor.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TgoExceptionReportMessage, ExceptionReportMessageHandler);
  inherited;
end;

procedure TExceptionInterceptor.AddHandler(const AHandler: TExceptionReportProc);
begin
  FHandlers := FHandlers + [AHandler];
end;

procedure TExceptionInterceptor.ExceptionReportMessageHandler(const Sender: TObject; const AMsg: TMessage);
var
  LReport: IgoExceptionReport;
  LHandler: TExceptionReportProc;
begin
  LReport := TgoExceptionReportMessage(AMsg).Report;
  if not LReport.ExceptionLocation.RoutineName.IsEmpty then
    TOSLog.e('%s caused: %s ', [LReport.ExceptionLocation.RoutineName, LReport.ExceptionMessage])
  else
    TOSLog.e(LReport.ExceptionMessage);
  LogCallstack(LReport.CallStack);
  for LHandler in FHandlers do
    LHandler(LReport);
end;

procedure TExceptionInterceptor.LogCallstack(const ACallstack: TgoCallStack);
var
  LEntry: TgoCallStackEntry;
  LLine: string;
begin
  for LEntry in ACallStack do
  begin
    if not LEntry.RoutineName.IsEmpty then
    begin
      if LEntry.LineNumber > 0 then
        LLine := Format('%s + %d, line %d', [LEntry.RoutineName, LEntry.CodeAddress - LEntry.RoutineAddress, LEntry.LineNumber])
      else
        LLine := Format('%s + %d', [LEntry.RoutineName, LEntry.CodeAddress - LEntry.RoutineAddress]);
    end
    else
      LLine := Format('%-25s %s', [ExtractFilename(LEntry.ModuleName), AddressToString(LEntry.CodeAddress)]);
    {$IF Defined(ANDROID)}
    TOSLog.e('> ' + LLine);
    {$ELSE}
    TOSLog.e('  at ' + LLine);
    {$ENDIF}
  end;
end;

initialization
  ExceptionInterceptor := TExceptionInterceptor.Create;

end.
