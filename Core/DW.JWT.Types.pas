unit DW.JWT.Types;

interface

type
  TJWTCreatorParams = record
    Algorithm: string;
    Audience: string;
    IssuerID: string;
    KeyID: string;
    Secret: string;
  end;

  IJWTCreator = interface(IInterface)
    ['{E9729F6A-7FFD-480A-8215-58534A8DE100}']
    function GetJWT(const AExpiration: Integer): string;
  end;

implementation

end.
