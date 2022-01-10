unit DW.MediaManager.Win;

{$I DW.GlobalDefines.inc}

interface

uses
  // DW
  DW.Photos;

type
  TPlatformPhotos = class(TCustomPlatformPhotos)
  public
    procedure RequestAuthorization; override;
  end;

  TPlatformPhotoCollections = class(TPhotoCollections)
  protected
    procedure DoReload; override;
  end;

implementation

uses
  // RTL
  System.IOUtils, System.StrUtils, System.SysUtils,
  // DW
  DW.Classes.Helpers, DW.Types;

type
  TPlatformPhoto = class(TPhoto)
  private
    procedure InternalDoLoad;
  protected
    FPath: string;
    procedure DoLoad(const AWidth, AHeight: Single); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TPlatformPhotoCollection = class(TPhotoCollection)
  private
    procedure AddPhoto(const APath: string);
    function GetPhotoFiles: TArray<string>;
  protected
    FPath: string;
    procedure DoReload(const ACount: Integer); override;
  public
    constructor Create(const AID, ATitle: string); override;
    destructor Destroy; override;
  end;

{ TPlatformPhoto }

constructor TPlatformPhoto.Create;
begin
  inherited;
  //
end;

destructor TPlatformPhoto.Destroy;
begin
  //
  inherited;
end;

procedure TPlatformPhoto.DoLoad(const AWidth, AHeight: Single);
begin
  TDo.RunSync(InternalDoLoad, DoLoaded);
end;

procedure TPlatformPhoto.InternalDoLoad;
begin
  ImageStream.LoadFromFile(FPath);
end;

{ TPlatformPhotoCollection }

constructor TPlatformPhotoCollection.Create(const AID, ATitle: string);
begin
  inherited;
  //
end;

destructor TPlatformPhotoCollection.Destroy;
begin
  //
  inherited;
end;

procedure TPlatformPhotoCollection.AddPhoto(const APath: string);
var
  LPhoto: TPlatformPhoto;
begin
  LPhoto := TPlatformPhoto.Create;
  LPhoto.FPath := APath;
  LPhoto.FID := Count.ToString;
  Add(LPhoto);
end;

procedure TPlatformPhotoCollection.DoReload(const ACount: Integer);
var
  LFiles: TArray<string>;
  LFile: string;
begin
  LFiles := GetPhotoFiles;
  for LFile in LFiles do
  begin
    if MatchText(TPath.GetExtension(LFile), ['.jpg', '.jpeg', '.bmp', '.png']) then
    begin
      if (ACount = 0) or (Count < ACount)  then
        AddPhoto(LFile);
    end;
  end;
end;

function TPlatformPhotoCollection.GetPhotoFiles: TArray<string>;
begin
  Result := TDirectory.GetFiles('Z:\Temp\Photos', '*.*', TSearchOption.soTopDirectoryOnly);
end;

{ TPlatformPhotoCollections }

procedure TPlatformPhotoCollections.DoReload;
var
  LCollection: TPhotoCollection;
begin
  LCollection := TPlatformPhotoCollection.Create('1', 'Camera Roll');
  Add(LCollection);
end;

{ TPlatformPhotos }

procedure TPlatformPhotos.RequestAuthorization;
begin
  AuthorizationStatus := TAuthorizationStatus.Authorized;
end;

end.
