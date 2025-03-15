unit DW.BroadcastMessage.Consts;

interface

uses
  Androidapi.JNI.JavaTypes;

type
  IBroadcastMessageConsts = interface(IInterface)
    ['{211B68AC-E621-4ECC-9CA0-D70FA48DB6C2}']
    function GetACTION_MESSAGE: JString;
    function GetEXTRA_MESSAGE: JString;
    function GetEXTRA_MESSAGE_KIND: JString;
    property ACTION_MESSAGE: JString read GetACTION_MESSAGE;
    property EXTRA_MESSAGE: JString read GetEXTRA_MESSAGE;
    property EXTRA_MESSAGE_KIND: JString read GetEXTRA_MESSAGE_KIND;
  end;

var
  BroadcastMessageConsts: IBroadcastMessageConsts;

implementation

uses
  Androidapi.Helpers;

type
  TBroadcastMessageConsts = class(TInterfacedObject, IBroadcastMessageConsts)
  private
    FACTION_MESSAGE: JString;
    FEXTRA_MESSAGE: JString;
    FEXTRA_MESSAGE_KIND: JString;
  public
    function GetACTION_MESSAGE: JString;
    function GetEXTRA_MESSAGE: JString;
    function GetEXTRA_MESSAGE_KIND: JString;
  end;

{ TBroadcastMessageConsts }

function TBroadcastMessageConsts.GetACTION_MESSAGE: JString;
begin
  if FACTION_MESSAGE = nil then
    FACTION_MESSAGE := StringToJString('ACTION_MESSAGE');
  Result := FACTION_MESSAGE;
end;

function TBroadcastMessageConsts.GetEXTRA_MESSAGE: JString;
begin
  if FEXTRA_MESSAGE = nil then
    FEXTRA_MESSAGE := StringToJString('EXTRA_MESSAGE');
  Result := FEXTRA_MESSAGE;
end;

function TBroadcastMessageConsts.GetEXTRA_MESSAGE_KIND: JString;
begin
  if FEXTRA_MESSAGE_KIND = nil then
    FEXTRA_MESSAGE_KIND := StringToJString('EXTRA_MESSAGE_KIND');
  Result := FEXTRA_MESSAGE_KIND;
end;

initialization
  BroadcastMessageConsts := TBroadcastMessageConsts.Create;

end.
