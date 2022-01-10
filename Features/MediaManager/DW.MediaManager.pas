unit DW.MediaManager;

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
  // RTL
  System.Classes, System.SysUtils, System.Generics.Collections, System.Messaging, System.Sensors,
  // FMX
  FMX.Graphics,
  // DW
  DW.Types;

type
  TMediaItem = class;

  TDeleteMediaItemResultProc = reference to procedure(const Error: string);
  TSaveMediaItemResultProc = reference to procedure(const Item: TMediaItem; const Error: string);
  TUpdateMediaItemResultProc = reference to procedure(const Error: string);

  TItemUpdateFlag = (DateTime, Location, Hidden, Favorite);

  TItemUpdateFlags = set of TItemUpdateFlag;

  TMediaItem = class(TObject)
  private
    FID: string;
    FDateTime: TDateTime;
    FFavorite: Boolean;
    FFileName: string;
    FHidden: Boolean;
    FImage: TMemoryStream;
    FLocation: TLocationCoord2D;
    FUpdateFlags: TItemUpdateFlags;
    function GetImage: TStream;
    procedure SetDateTime(const Value: TDateTime);
    procedure SetFavorite(const Value: Boolean);
    procedure SetHidden(const Value: Boolean);
    procedure SetLocation(const Value: TLocationCoord2D);
  protected
    function GetOriginalFileName: string; virtual;
    procedure OptionsChanged; virtual;
    property ImageStream: TMemoryStream read FImage;
    property UpdateFlags: TItemUpdateFlags read FUpdateFlags write FUpdateFlags;
  public
    constructor Create(const AID, AFileName: string); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    /// <summary>
    ///   Asynchronously deletes the item
    /// </summary>
    procedure Delete(const AResultHandler: TDeleteMediaItemResultProc); virtual;
    /// <summary>
    ///   Asynchronously loads the asset
    /// </summary>
    procedure Load(const ALoadedHandler: TProc; const AWidth: Single = 0; const AHeight: Single = 0); virtual;
    /// <summary>
    ///   Asynchronously updates the assets properties
    /// </summary>
    /// <remarks>
    ///   Properties must be changed before calling this method
    /// </remarks>
    procedure UpdateProperties(const AResultHandler: TUpdateMediaItemResultProc); virtual;
    /// <summary>
    ///   Date/time that the asset was added
    /// </summary>
    property DateTime: TDateTime read FDateTime write SetDateTime;
    /// <summary>
    ///   Indicates that the user has added the asset to their favorites
    /// </summary>
    /// <remarks>
    ///   Applies to iOS only
    /// </remarks>
    property Favorite: Boolean read FFavorite write SetFavorite;
    /// <summary>
    ///   File name (without path) associated with the asset
    /// </summary>
    property FileName: string read FFileName;
    /// <summary>
    ///   Indicates that the user has hidden the asset
    /// </summary>
    /// <remarks>
    ///   Applies to iOS only
    /// </remarks>
    property Hidden: Boolean read FHidden write SetHidden;
    /// <summary>
    ///   Uniquely identifies the asset
    /// </summary>
    property ID: string read FID;
    /// <summary>
    ///   Asset image, as a stream
    /// </summary>
    property Image: TStream read GetImage;
    /// <summary>
    ///   The location at which the asset was created
    /// </summary>
    property Location: TLocationCoord2D read FLocation write SetLocation;
    /// <summary>
    ///   The filename with which the asset was added
    /// </summary>
    property OriginalFileName: string read GetOriginalFileName;
  end;

  /// <summary>
  ///   Manages collections of assets
  /// </summary>
  TMediaCollection = class(TObjectList<TMediaItem>)
  private
    // FCollectionType: TCollectionType;
    FID: string;
    FTitle: string;
  protected
    procedure DoLoad; virtual;
  public
    constructor Create(const AID, ATitle: string); virtual;
    destructor Destroy; override;
    /// <summary>
    ///   Finds the asset with the given ID
    /// </summary>
    function FindAssetByID(const AID: string; out AAsset: TMediaItem): Boolean;
    /// <summary>
    ///   Loads the IDs of the assets - i.e. does not load the actual images
    /// </summary>
    procedure Load;
    /// <summary>
    ///   Asynchronously saves an asset into the collection, from the given filename
    /// </summary>
    procedure SaveItem(const AFileName: string; const AResultHandler: TSaveMediaItemResultProc); virtual;
    /// <summary>
    ///   Uniquely identifies the asset collection
    /// </summary>
    property ID: string read FID;
    /// <summary>
    ///   Display name of the collection
    /// </summary>
    property Title: string read FTitle;
  end;

  TNewCollectionResultProc = reference to procedure(const Collection: TMediaCollection; const Error: string);

  /// <summary>
  ///   Manages collections of media item collections
  /// </summary>
  TMediaCollections = class(TObjectList<TMediaCollection>)
  private
    FCollectionSubtypes: TArray<LongInt>;
    FCollectionTypes: TArray<LongInt>;
    function GetItem(const AIndex: Integer): TMediaCollection;
  protected
    FNewCollectionResultHandler: TNewCollectionResultProc;
    procedure DoLoad; virtual;
    function IndexOfID(const AID: string): Integer;
  public
    /// <summary>
    ///   Loads the collections
    /// </summary>
    procedure Load;
    /// <summary>
    ///   Indicates whether a collection with the given title exists
    /// </summary>
    function CollectionTitleExists(const ATitle: string): Boolean; virtual;
    /// <summary>
    ///   Asynchronously creates a new collection with the given title
    /// </summary>
    procedure NewCollection(const ATitle: string; const AResultHandler: TNewCollectionResultProc); virtual;
    property CollectionSubtypes: TArray<LongInt> read FCollectionSubtypes write FCollectionSubtypes;
    property CollectionTypes: TArray<LongInt> read FCollectionTypes write FCollectionTypes;
    property Items[const AIndex: Integer]: TMediaCollection read GetItem; default;
  end;

  TMediaManager = class;

  /// <summary>
  ///   Ancestor for platform-specific assets managers. Manages a list of collections
  /// </summary>
  TCustomPlatformMediaManager = class(TObject)
  private
    FCollections: TMediaCollections;
    FMediaManager: TMediaManager;
  protected
    procedure DeleteItems(const AAssets: TArray<TMediaItem>; const AResultHandler: TDeleteMediaItemResultProc); virtual;
    procedure DoAuthorizationStatus;
    function GetAuthorizationStatus: TAuthorizationStatus; virtual;
    function GetCameraRoll: TMediaCollection; virtual;
    procedure RequestPermission; virtual;
    property Collections: TMediaCollections read FCollections;
    property MediaManager: TMediaManager read FMediaManager;
  public
    constructor Create(const AMediaManager: TMediaManager); virtual;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   Manager of collections of asset collections
  /// </summary>
  TMediaManager = class(TObject)
  private
    FPlatformMediaManager: TCustomPlatformMediaManager;
    FOnAuthorizationStatus: TAuthorizationStatusEvent;
    function GetAuthorizationStatus: TAuthorizationStatus;
    function GetCollections: TMediaCollections;
    function GetCameraRoll: TMediaCollection;
  protected
    procedure DoAuthorizationStatus;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    ///   Asynchronously deletes the given items
    /// </summary>
    procedure DeleteItems(const AItems: TArray<TMediaItem>; const AResultHandler: TDeleteMediaItemResultProc);
    /// <summary>
    ///   Requests permission to access assets on the device
    /// </summary>
    /// <remarks>
    ///   Use the OnAuthorizationStatus event to determine whether or not access was granted
    /// </remarks>
    procedure RequestPermission;
    property AuthorizationStatus: TAuthorizationStatus read GetAuthorizationStatus;
    /// <summary>
    ///   Returns what is considered to be the camera collection of assets
    /// </summary>
    property CameraRoll: TMediaCollection read GetCameraRoll;
    /// <summary>
    ///   Returns a collection of asset collections (otherwise known as albums)
    /// </summary>
    property Collections: TMediaCollections read GetCollections;
    property OnAuthorizationStatus: TAuthorizationStatusEvent read FOnAuthorizationStatus write FOnAuthorizationStatus;
  end;

implementation

uses
  // RTL
  System.Types, System.IOUtils,
  // FMX
  FMX.Types,
  // DW
{$IF Defined(IOS)}
  DW.MediaManager.iOS;
{$ELSEIF Defined(ANDROID)}
  DW.MediaManager.Android;
{$ELSEIF Defined(MSWINDOWS)}
  DW.MediaManager.Win;
{$ELSE}
  DW.MediaManager.Default;
{$ENDIF}

{ TMediaItem }

constructor TMediaItem.Create(const AID, AFileName: string);
begin
  inherited Create;
  FID := AID;
  FFileName := AFileName;
  FImage := TMemoryStream.Create;
end;

procedure TMediaItem.Delete(const AResultHandler: TDeleteMediaItemResultProc);
begin
  //
end;

destructor TMediaItem.Destroy;
begin
  FImage.Free;
  inherited;
end;

procedure TMediaItem.Load(const ALoadedHandler: TProc; const AWidth: Single = 0; const AHeight: Single = 0);
begin
  //
end;

procedure TMediaItem.AfterConstruction;
begin
  inherited;
  OptionsChanged;
end;

procedure TMediaItem.OptionsChanged;
begin
  //
end;

procedure TMediaItem.SetDateTime(const Value: TDateTime);
begin
  if Value <> FDateTime then
  begin
    FDateTime := Value;
    Include(FUpdateFlags, TItemUpdateFlag.DateTime);
  end;
end;

procedure TMediaItem.SetFavorite(const Value: Boolean);
begin
  if Value <> FFavorite then
  begin
    FFavorite := Value;
    Include(FUpdateFlags, TItemUpdateFlag.Favorite);
  end;
end;

procedure TMediaItem.SetHidden(const Value: Boolean);
begin
  if Value <> FHidden then
  begin
    FHidden := Value;
    Include(FUpdateFlags, TItemUpdateFlag.Hidden);
  end;
end;

procedure TMediaItem.SetLocation(const Value: TLocationCoord2D);
begin
  if (Value.Latitude <> FLocation.Latitude) or (Value.Longitude <> FLocation.Longitude) then
  begin
    FLocation := Value;
    Include(FUpdateFlags, TItemUpdateFlag.Location);
  end;
end;

procedure TMediaItem.UpdateProperties(const AResultHandler: TUpdateMediaItemResultProc);
begin
  //
end;

function TMediaItem.GetImage: TStream;
begin
  Result := FImage;
end;

function TMediaItem.GetOriginalFileName: string;
begin
  Result := '';
end;

{ TMediaCollection }

constructor TMediaCollection.Create(const AID, ATitle: string);
begin
  inherited Create;
  FID := AID;
  FTitle := ATitle;
end;

destructor TMediaCollection.Destroy;
begin
  //
  inherited;
end;

procedure TMediaCollection.DoLoad;
begin
  //
end;

function TMediaCollection.FindAssetByID(const AID: string; out AAsset: TMediaItem): Boolean;
var
  LAsset: TMediaItem;
begin
  Result := False;
  for LAsset in Self do
  begin
    if LAsset.ID.Equals(AID) then
    begin
      AAsset := LAsset;
      Exit(True);
    end;
  end;
end;

procedure TMediaCollection.Load;
begin
  Clear;
  DoLoad;
end;

procedure TMediaCollection.SaveItem(const AFileName: string; const AResultHandler: TSaveMediaItemResultProc);
begin
  //
end;

{ TCustomPlatformMediaManager }

constructor TCustomPlatformMediaManager.Create(const AMediaManager: TMediaManager);
begin
  inherited Create;
  FMediaManager := AMediaManager;
  FCollections := TPlatformMediaCollections.Create;
end;

procedure TCustomPlatformMediaManager.DeleteItems(const AAssets: TArray<TMediaItem>; const AResultHandler: TDeleteMediaItemResultProc);
begin
  //
end;

destructor TCustomPlatformMediaManager.Destroy;
begin
  FCollections.Free;
  inherited;
end;

procedure TCustomPlatformMediaManager.DoAuthorizationStatus;
begin
  FMediaManager.DoAuthorizationStatus;
end;

function TCustomPlatformMediaManager.GetAuthorizationStatus: TAuthorizationStatus;
begin
  Result := TAuthorizationStatus.NotDetermined;
end;

function TCustomPlatformMediaManager.GetCameraRoll: TMediaCollection;
begin
  // Default implementation assumes first in the collections
  Result := nil;
  if FCollections.Count > 0 then
    Result := FCollections[0];
end;

procedure TCustomPlatformMediaManager.RequestPermission;
begin
  //
end;

{ TMediaCollections }

function TMediaCollections.CollectionTitleExists(const ATitle: string): Boolean;
begin
  Result := False;
end;

procedure TMediaCollections.DoLoad;
begin
  //
end;

function TMediaCollections.GetItem(const AIndex: Integer): TMediaCollection;
begin
  Result := inherited Items[AIndex];
end;

function TMediaCollections.IndexOfID(const AID: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    if Items[I].ID.Equals(AID) then
      Exit(I);
  end;
end;

procedure TMediaCollections.Load;
begin
  Clear;
  DoLoad;
end;

procedure TMediaCollections.NewCollection(const ATitle: string; const AResultHandler: TNewCollectionResultProc);
begin
  //
end;

{ TMediaManager }

constructor TMediaManager.Create;
begin
  inherited;
  FPlatformMediaManager := TPlatformMediaManager.Create(Self);
end;

procedure TMediaManager.DeleteItems(const AItems: TArray<TMediaItem>; const AResultHandler: TDeleteMediaItemResultProc);
begin
  FPlatformMediaManager.DeleteItems(AItems, AResultHandler);
end;

destructor TMediaManager.Destroy;
begin
  FPlatformMediaManager.Free;
  inherited;
end;

procedure TMediaManager.DoAuthorizationStatus;
begin
  if Assigned(FOnAuthorizationStatus) then
    FOnAuthorizationStatus(Self, GetAuthorizationStatus);
end;

function TMediaManager.GetAuthorizationStatus: TAuthorizationStatus;
begin
  Result := FPlatformMediaManager.GetAuthorizationStatus;
end;

function TMediaManager.GetCameraRoll: TMediaCollection;
begin
  Result := FPlatformMediaManager.GetCameraRoll;
end;

function TMediaManager.GetCollections: TMediaCollections;
begin
  Result := FPlatformMediaManager.Collections;
end;

procedure TMediaManager.RequestPermission;
begin
  FPlatformMediaManager.RequestPermission;
end;

end.
