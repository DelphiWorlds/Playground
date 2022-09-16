unit LD.Service;

interface

implementation

uses
  System.SysUtils,
  AndroidApi.JNI, Androidapi.JNI.JavaTypes,
  DW.JNIExport;

// Thanks, Simon Choi

const
  cFuncPrefix = 'Java_com_delphiworlds_LocationDemo_ServiceLibrary_';

//var
//  Inx: Integer;

function Test(env: PJNIEnv; this: JNIObject; value: integer): Integer; cdecl;
begin
  Debug('LD.Service', 'Test');
//  Result := Value + Inx;
//  Inc(Inx);
  Result := 0;
end;

procedure ReceivedLocation(env: PJNIEnv; this: JNIObject; json: JNIString); cdecl;
var
  LJSON: string;
  M: TMarshaller;
begin
  Debug('LD.Service', 'ReceivedLocation');
  if System.JavaContext = nil then
  begin
    env^.GetJavaVM(env, @System.JavaMachine);
    System.JavaContext := env^.NewGlobalRef(env, this);
    Debug('LD.Service', 'Assigned context');
  end;
  if json <> nil then
  begin
    Debug('LD.Service','json is not nil');
    LJSON := JNIStringToString(env, json); // <------- Goes BOOM! Here
    Debug('LD.Service', M.AsAnsi(LJSON).ToPointer);
  end
  else
    Debug('LD.Service','json is nil');
end;

exports
  Test name cFuncPrefix + 'Test',
  ReceivedLocation name cFuncPrefix + 'ReceivedLocation';

end.
