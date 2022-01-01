unit LD.LocationsDataModule;

interface

uses
  System.SysUtils, System.Classes, System.Sensors,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.ConsoleUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.Phys.SQLiteWrapper.Stat,
  DW.Location, DW.Location.Types;

type
  TLocationsDataModule = class(TDataModule)
    FDConnection: TFDConnection;
    LocationsTable: TFDTable;
    LocationsTableID: TFDAutoIncField;
    LocationsTableLatitude: TFloatField;
    LocationsTableLongitude: TFloatField;
    LocationsTableDeviceState: TIntegerField;
  protected
    function Connect: Boolean;
    procedure AppendLocation(const AData: TLocationData);
  public
    class procedure AddLocation(const AData: TLocationData);
  end;

var
  LocationsDataModule: TLocationsDataModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  System.IOUtils,
  DW.OSLog;

{ TLocationsDataModule }

class procedure TLocationsDataModule.AddLocation(const AData: TLocationData);
var
  LDataModule: TLocationsDataModule;
begin
  LDataModule := TLocationsDataModule.Create(nil);
  try
    if LDataModule.Connect then
      LDataModule.AppendLocation(AData);
  finally
    LDataModule.Free;
  end;
end;

function TLocationsDataModule.Connect: Boolean;
begin
  Result := False;
  FDConnection.Params.Database := TPath.Combine(TPath.GetDocumentsPath, 'LD.sqlite');
  try
    FDConnection.Connected := True;
  except
    on E: Exception do
      TOSLog.e('Unable to connect to %s - %s: %s', [FDConnection.Params.Database, E.ClassName, E.Message]);
  end;
  if FDConnection.Connected then
  try
    LocationsTable.Open;
    Result := LocationsTable.Active;
  except
    on E: Exception do
      TOSLog.e('Unable to open Locations table - %s: %s', [E.ClassName, E.Message]);
  end;
end;

procedure TLocationsDataModule.AppendLocation(const AData: TLocationData);
begin
  if LocationsTable.Active then
  try
    LocationsTable.Append;
    LocationsTableLatitude.Value := AData.Location.Latitude;
    LocationsTableLongitude.Value := AData.Location.Longitude;
    LocationsTableDeviceState.Value := Ord(AData.ApplicationState);
    LocationsTable.Post;
    TOSLog.d('Posted record to database');
  except
    on E: Exception do
      TOSLog.e('Unable to add record to Locations table - %s: %s', [E.ClassName, E.Message]);
  end;
end;

end.
