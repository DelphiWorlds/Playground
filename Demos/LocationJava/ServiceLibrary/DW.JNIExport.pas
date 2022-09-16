unit DW.JNIExport;

interface

uses
  System.SysUtils, System.Generics.Collections,
  AndroidApi.JNI,
  Androidapi.Log, // Debug
  Posix.Dlfcn, Posix.Stdlib;

type
  TLibrary = class abstract
  public
    class var FHandles: TDictionary<string, NativeUInt>;
    class function FindHandle(const LibName: string): NativeUInt; static;
    class procedure UnLoad(const LibName: string); static;
  end;

procedure Debug(const ATag, AMsg: MarshaledAString);

implementation

procedure Debug(const ATag, AMsg: MarshaledAString);
begin
  __android_log_write(ANDROID_LOG_DEBUG, ATag, AMsg);
end;

function JNI_OnLoad(PVM: PJavaVM; Reserved: Pointer): JNIInt; cdecl;
begin
  TLibrary.FHandles := TDictionary<string, NativeUInt>.Create;
  Result := JNI_VERSION_1_6;
end;

procedure JNI_OnUnload(PVM: PJavaVM; Reserved: Pointer); cdecl;
var
  LHandle: NativeUInt;
begin
  for LHandle in TLibrary.FHandles.Values do
    dlclose(LHandle);
  TLibrary.FHandles.Free;
end;

class function TLibrary.FindHandle(const LibName: string): NativeUInt;
var
  CStr: MarshaledAString;

  function StrToCStr(const Str: string; Dest: MarshaledAString): MarshaledAString;  // Only ASCII chars
  var
    I: Integer;
  begin
    Result := Dest;
    for I := Low(Str) to High(Str) do
    begin
      Byte(Dest^) := Byte(Str[I]);
      Inc(Dest);
    end;
    Byte(Dest^) := 0;
  end;

begin
  if FHandles = nil then
    FHandles := TDictionary<string, NativeUInt>.Create;
  if not FHandles.TryGetValue(LibName, Result) then
  begin
    CStr := System.AllocMem(512);
    Result := dlopen(StrToCStr(LibName, CStr), RTLD_LAZY);
    if Result > 0 then
      FHandles.Add(LibName, Result);
    System.FreeMem(CStr);
  end;
end;

class procedure TLibrary.UnLoad(const LibName: string);
var
  LHandle: NativeUInt;
begin
  if FHandles.TryGetValue(LibName, LHandle) then
  begin
    dlclose(LHandle);
    FHandles.Remove(LibName);
  end;
end;

exports
  JNI_OnLoad name 'JNI_OnLoad',
  JNI_OnUnload name 'JNI_OnUnload';

end.
