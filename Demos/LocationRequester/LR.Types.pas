unit LR.Types;

interface

uses
  System.JSON, System.Sensors;

type
  TLocationRef = record
    Name: string;
    Latitude: Double;
    Longitude: Double;
    constructor Create(const AJSONValue: TJSONValue);
    function GetLocation: TLocationCoord2D;
  end;

  TLocationRefs = TArray<TLocationRef>;

  TLocationRefsConfig = record
    LocationRefs: TLocationRefs;
    function GetLocations: TArray<TLocationCoord2D>;
    procedure Load;
  end;

implementation

uses
  System.IOUtils, System.SysUtils;

{ TLocationRef }

constructor TLocationRef.Create(const AJSONValue: TJSONValue);
begin
  AJSONValue.TryGetValue('Name', Name);
  AJSONValue.TryGetValue('Latitude', Latitude);
  AJSONValue.TryGetValue('Longitude', Longitude);
end;

function TLocationRef.GetLocation: TLocationCoord2D;
begin
  Result := TLocationCoord2D.Create(Latitude, Longitude);
end;

{ TLocationRefsConfig }

function TLocationRefsConfig.GetLocations: TArray<TLocationCoord2D>;
var
  LRef: TLocationRef;
begin
  Result := [];
  for LRef in LocationRefs do
    Result := Result + [LRef.GetLocation];
end;

procedure TLocationRefsConfig.Load;
var
  LFileName: string;
  LJSON, LRef: TJSONValue;
  LRefs: TJSONArray;
begin
  LocationRefs := [];
  LFileName := TPath.Combine(TPath.GetDocumentsPath, 'locationrefs.json');
  if TFile.Exists(LFileName) then
  begin
    LJSON := TJSONObject.ParseJSONValue(TFile.ReadAllText(LFileName));
    if LJSON <> nil then
    try
      if LJSON.TryGetValue('Refs', LRefs) then
      begin
        for LRef in LRefs do
          LocationRefs := LocationRefs + [TLocationRef.Create(LRef)];
      end;
    finally
      LJSON.Free;
    end;
  end;
end;

end.
