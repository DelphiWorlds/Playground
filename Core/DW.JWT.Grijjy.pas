unit DW.JWT.Grijjy;

interface

uses
  DW.JWT.Types,
  Grijjy.OAuth2;

type
  TJWTCreator = class(TInterfacedObject, IJWTCreator)
  private
    FParams: TJWTCreatorParams;
    function FindAlgorithm(const AName: string; out AAlgorithm: TgoAlgorithm): Boolean;
  public
    { IJWTCreator }
    function GetJWT(const AExpiration: Integer): string;
  public
    constructor Create(const AParams: TJWTCreatorParams);
  end;

implementation

uses
  System.SysUtils;

{ TJWTCreator }

constructor TJWTCreator.Create(const AParams: TJWTCreatorParams);
begin
  inherited Create;
  FParams := AParams;
  FParams.Algorithm := FParams.Algorithm.ToUpper;
end;

function TJWTCreator.FindAlgorithm(const AName: string; out AAlgorithm: TgoAlgorithm): Boolean;
begin
  Result := False;
  if AName.Equals('RS256') then
  begin
    AAlgorithm := TgoAlgorithm.RS256;
    Result := True;
  end
  else if AName.Equals('ES256') then
  begin
    AAlgorithm := TgoAlgorithm.ES256;
    Result := True;
  end;
end;

function TJWTCreator.GetJWT(const AExpiration: Integer): string;
var
  LJWT: TgoJWT;
  LClaims: TgoOAuth2ClaimSet;
  LHeader: TgoOAuth2Header;
  LAlgorithm: TgoAlgorithm;
begin
  Result := '';
  if FindAlgorithm(FParams.Algorithm, LAlgorithm) then
  begin
    LClaims.Aud := FParams.Audience;
    LClaims.Iss := FParams.IssuerID;
    LClaims.Iat := Now;
    LHeader.Alg := FParams.Algorithm;
    LHeader.Typ := 'JWT';
    LHeader.Kid := FParams.KeyID;
    LJWT.Initialize(LHeader.ToJson, LClaims.ToJson(AExpiration));
    LJWT.Sign(TEncoding.UTF8.GetBytes(FParams.Secret), Result, LAlgorithm);
  end;
end;

end.
