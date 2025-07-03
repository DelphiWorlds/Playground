unit DW.LocationService.Common;

interface

uses
  System.JSON,
  Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  DW.FusedLocation;

type
  TLocationServiceInfo = record
  private
    function ACTION_SERVICE_INFO: JString;
    function EXTRA_ACTIVE: JString;
    function EXTRA_OPTIONS: JString;
  public
    IsActive: Boolean;
    IsServiceInfo: Boolean;
    Options: TFusedLocationOptions;
    constructor Create(const AIntent: JIntent);
    procedure FromIntent(const AIntent: JIntent);
    function ToIntent: JIntent;
  end;

  ILocationPreferences = interface(IInterface)
    ['{37ECABDA-2698-44E4-9D40-78DB7F587576}']
    function GetIsActive: Boolean;
    function GetOptions: TFusedLocationOptions;
  end;

  TLocationPreferences = class(TInterfacedObject, ILocationPreferences)
  protected
    function GetPreferences: JSharedPreferences;
  public
    { ILocationPreferences }
    function GetIsActive: Boolean;
    function GetOptions: TFusedLocationOptions;
  end;

  TFusedLocationOptionsHelper = record helper for TFusedLocationOptions
    procedure FromJSON(const AJSON: string);
    function ToJSON: string;
  end;

implementation

uses
  Androidapi.Helpers,
  DW.Androidapi.JNI.DWFusedLocation;

{ TFusedLocationOptionsHelper }

procedure TFusedLocationOptionsHelper.FromJSON(const AJSON: string);
var
  LValue: TJSONValue;
begin
  LValue := TJSONObject.ParseJSONValue(AJSON);
  if LValue <> nil then
  try
    LValue.TryGetValue('FastestInterval', FastestInterval);
    LValue.TryGetValue('Interval', Interval);
    LValue.TryGetValue('Priority', Priority);
    LValue.TryGetValue('SmallestDisplacement', SmallestDisplacement);
  finally
    LValue.Free;
  end;
end;

function TFusedLocationOptionsHelper.ToJSON: string;
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('FastestInterval', FastestInterval);
    LJSON.AddPair('Interval', Interval);
    LJSON.AddPair('Priority', Priority);
    LJSON.AddPair('SmallestDisplacement', SmallestDisplacement);
    Result := LJSON.ToJSON;
  finally
    LJSON.Free;
  end;
end;

{ TLocationPreferences }

function TLocationPreferences.GetIsActive: Boolean;
begin
  Result := GetPreferences.getBoolean(StringToJString('IsActive'), False);
end;

function TLocationPreferences.GetOptions: TFusedLocationOptions;
var
  LPreferences: JSharedPreferences;
begin
  LPreferences := GetPreferences;
  Result.FastestInterval := LPreferences.getLong(StringToJString('FastestInterval'), 0);
  Result.Interval := LPreferences.getLong(StringToJString('Interval'), 0);
  Result.Priority := LPreferences.getInt(StringToJString('Priority'), 0);
  Result.SmallestDisplacement := LPreferences.getFloat(StringToJString('SmallestDisplacement'), 0);
end;

function TLocationPreferences.GetPreferences: JSharedPreferences;
var
  LName: JString;
begin
  LName := TJDWFusedLocationClient.JavaClass.getPreferencesName(TAndroidHelper.Context);
  Result := TAndroidHelper.Context.getSharedPreferences(LName, TJContext.JavaClass.MODE_PRIVATE);
end;

{ TLocationServiceInfo }

constructor TLocationServiceInfo.Create(const AIntent: JIntent);
begin
  FromIntent(AIntent);
end;

function TLocationServiceInfo.ACTION_SERVICE_INFO: JString;
begin
  Result := StringToJString('ACTION_SERVICE_INFO');
end;

function TLocationServiceInfo.EXTRA_ACTIVE: JString;
begin
  Result := StringToJString('EXTRA_ACTIVE');
end;

function TLocationServiceInfo.EXTRA_OPTIONS: JString;
begin
  Result := StringToJString('EXTRA_OPTIONS');
end;

procedure TLocationServiceInfo.FromIntent(const AIntent: JIntent);
begin
  IsServiceInfo := (AIntent.getAction <> nil) and AIntent.getAction.equals(ACTION_SERVICE_INFO);
  if IsServiceInfo then
  begin
    IsActive := AIntent.getIntExtra(EXTRA_ACTIVE, 0) <> 0;
    if AIntent.hasExtra(EXTRA_OPTIONS) then
      Options.FromJSON(JStringToString(AIntent.getStringExtra(EXTRA_OPTIONS)));
  end;
end;

function TLocationServiceInfo.ToIntent: JIntent;
var
  LValue: Integer;
begin
  Result := TJIntent.JavaClass.init(ACTION_SERVICE_INFO);
  LValue := Ord(IsActive);
  Result.putExtra(EXTRA_ACTIVE, LValue); // Boolean doesn't work??
  Result.putExtra(EXTRA_OPTIONS, StringToJString(Options.ToJSON));
end;

end.
