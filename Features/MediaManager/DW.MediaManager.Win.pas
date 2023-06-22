unit DW.MediaManager.Win;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // DW
  DW.MediaManager, DW.Types;

type
  TPlatformMediaManager = class(TCustomPlatformMediaManager)
  protected
    function GetAuthorizationStatus: TAuthorizationStatus; override;
    procedure RequestPermission; override;
  end;

  TPlatformMediaCollections = class(TMediaCollections)
  protected
    procedure DoLoad; override;
  end;

implementation

uses
  // RTL
  System.IOUtils, System.StrUtils, System.SysUtils, System.Classes,
  // DW
  DW.Classes.Helpers;

type
  TPlatformMediaItem = class(TMediaItem)
  private
    procedure InternalDoLoad;
  protected
    FPath: string;
  public
    constructor Create(const AID, AFileName: string); override;
    destructor Destroy; override;
    procedure Load(const ALoadedHandler: TProc; const AWidth: Single = 0; const AHeight: Single = 0); override;
  end;

  TPlatformMediaCollection = class(TMediaCollection)
  private
    procedure AddItem(const APath: string);
    function GetFiles: TArray<string>;
  protected
    FPath: string;
    procedure DoLoad; override;
  public
    constructor Create(const AID, ATitle: string); override;
    destructor Destroy; override;
  end;

{ TPlatformMediaItem }

constructor TPlatformMediaItem.Create;
begin
  inherited;
  //
end;

destructor TPlatformMediaItem.Destroy;
begin
  //
  inherited;
end;

procedure TPlatformMediaItem.Load(const ALoadedHandler: TProc; const AWidth: Single = 0; const AHeight: Single = 0);
begin
  TDo.RunSync(InternalDoLoad, procedure begin ALoadedHandler end);
end;

procedure TPlatformMediaItem.InternalDoLoad;
begin
  ImageStream.LoadFromFile(FPath);
end;

{ TPlatformMediaCollection }

constructor TPlatformMediaCollection.Create(const AID, ATitle: string);
begin
  inherited;
  //
end;

destructor TPlatformMediaCollection.Destroy;
begin
  //
  inherited;
end;

procedure TPlatformMediaCollection.AddItem(const APath: string);
var
  LItem: TPlatformMediaItem;
begin
  LItem := TPlatformMediaItem.Create(Count.ToString, APath);
  Add(LItem);
end;

procedure TPlatformMediaCollection.DoLoad;
var
  LFiles: TArray<string>;
  LFile: string;
begin
  LFiles := GetFiles;
  for LFile in LFiles do
  begin
    if MatchText(TPath.GetExtension(LFile), ['.jpg', '.jpeg', '.bmp', '.png']) then
      AddItem(LFile);
  end;
end;

function TPlatformMediaCollection.GetFiles: TArray<string>;
begin
  Result := TDirectory.GetFiles(TPath.GetSharedPicturesPath, '*.*', TSearchOption.soTopDirectoryOnly);
end;

{ TPlatformMediaCollections }

procedure TPlatformMediaCollections.DoLoad;
var
  LCollection: TMediaCollection;
begin
  LCollection := TPlatformMediaCollection.Create('1', 'Camera Roll');
  Add(LCollection);
end;

{ TPlatformMediaManager }

function TPlatformMediaManager.GetAuthorizationStatus: TAuthorizationStatus;
begin
  Result := TAuthorizationStatus.Authorized;
end;

procedure TPlatformMediaManager.RequestPermission;
begin
  DoAuthorizationStatus;
end;

end.
