unit DW.MediaManager.iOS;

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
  System.SysUtils,
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation,
  // DW
  DW.MediaManager, DW.iOSapi.Photos, DW.Types;

type
  TFetchOptions = class(TObject)
  private
    FOptions: PHFetchOptions;
    FSortDescriptors: NSMutableArray;
    FPredicate: string;
    procedure SetPredicate(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddSortDescriptor(const AKey: string; const AAscending: Boolean);
    procedure Clear;
    property Predicate: string read FPredicate write SetPredicate;
    property Options: PHFetchOptions read FOptions;
  end;

  TPlatformMediaManager = class;

  TChangeObserver = class(TOCLocal, PHPhotoLibraryChangeObserver)
  private
    FMediaManager: TPlatformMediaManager;
  public
    { PHPhotoLibraryChangeObserver }
    procedure photoLibraryDidChange(changeInstance: PHChange); cdecl;
  public
    constructor Create(const AMediaManager: TPlatformMediaManager);
  end;

  TPlatformMediaManager = class(TCustomPlatformMediaManager)
  private
    FChangeObserver: TChangeObserver;
    procedure RequestAuthorizationHandler(AStatus: PHAuthorizationStatus);
  protected
    procedure DeleteItems(const AItems: TArray<TMediaItem>; const AResultHandler: TDeleteMediaItemResultProc); override;
    procedure DoChanges(const AChange: PHChange);
    function GetAuthorizationStatus: TAuthorizationStatus; override;
    procedure RequestPermission; override;
  public
    constructor Create(const AMediaManager: TMediaManager); override;
    destructor Destroy; override;
  end;

  TPlatformMediaCollections = class(TMediaCollections)
  private
    FFetchOptions: TFetchOptions;
    procedure FetchAlbums(const AAlbumType: PHAssetCollectionType; const AAlbumSubtype: PHAssetCollectionSubtype);
    function InternalCollectionTitleExists(const AAlbumType: PHAssetCollectionType; const ATitle: string): Boolean;
  protected
    procedure DoChanges(const AChange: PHChange);
    procedure DoLoad; override;
  public
    constructor Create;
    destructor Destroy; override;
    function CollectionTitleExists(const ATitle: string): Boolean; override;
    procedure NewCollection(const ATitle: string; const AResultHandler: TNewCollectionResultProc); override;
  end;

implementation

uses
  DW.OSLog,
  // RTL
  System.Classes, System.Sensors,
  // macOS
  Macapi.CoreFoundation, Macapi.Helpers,
  // iOS
  iOSapi.UIKit, iOSapi.CoreGraphics, iOSapi.CoreImage, iOSapi.CoreLocation, iOSapi.Helpers,
  // DW.OSLog,
  DW.Macapi.Helpers, DW.iOSapi.ImageIO, DW.Macapi.ObjCBlocks;

type
  TPlatformMediaCollection = class;

  TPlatformMediaItem = class(TMediaItem)
  private
    FCollection: TPlatformMediaCollection;
    FOptions: PHImageRequestOptions;
  protected
    function GetOriginalFileName: string; override;
    procedure OptionsChanged; override;
    property Collection: TPlatformMediaCollection read FCollection;
    property Options: PHImageRequestOptions read FOptions;
  public
    constructor Create(const ACollection: TPlatformMediaCollection; const ANativeAsset: PHAsset); reintroduce;
    destructor Destroy; override;
    procedure Delete(const AResultHandler: TDeleteMediaItemResultProc); override;
    procedure Load(const ALoadedHandler: TProc; const AWidth: Single = 0; const AHeight: Single = 0); override;
    procedure UpdateProperties(const AResultHandler: TUpdateMediaItemResultProc); override;
  end;

  TLoadItemRequest = class;

  TItemLoadCompleteProc = reference to procedure(Request: TLoadItemRequest);

  TLoadItemRequest = class(TObject)
  private
    FCompleteHandler: TItemLoadCompleteProc;
    FData: NSData;
    FItem: TPlatformMediaItem;
    FRequestedHeight: Single;
    FRequestedWidth: Single;
    procedure ContentEditingInputRequest(contentEditingInput: PHContentEditingInput; info: NSDictionary);
    procedure ImageDataForMediaItemResult(imageData: NSData; dataUTI: NSString; orientation: UIImageOrientation; info: NSDictionary);
    procedure ImageForMediaItemResult(image: UIImage; info: NSDictionary);
    procedure RequestImage(const ANativeAsset: PHAsset);
  protected
    procedure LoadItem(const AHandler: TItemLoadCompleteProc);
    property Data: NSData read FData;
  public
    constructor Create(const AItem: TPlatformMediaItem; const AWidth, AHeight: Single);
  end;

  TItemChangeRequest = class;

  TItemChangeRequestCompleteProc = reference to procedure(Request: TItemChangeRequest);

  TItemChangeRequest = class(TObject)
  private
    FCompleteHandler: TItemChangeRequestCompleteProc;
    FDoChangesBlock: Pointer;
    FError: string;
    procedure DoRequestComplete;
    procedure RequestCompletionHandler(success: Boolean; error: NSError);
  protected
    procedure DoExecute; virtual; abstract;
    procedure DoRequestCompletion; virtual;
    procedure PerformChanges(const AChangesProc: TProc);
    procedure Execute;
    property Error: string read FError;
  public
    constructor Create(const AHandler: TItemChangeRequestCompleteProc);
  end;

  TNewCollectionRequest = class(TItemChangeRequest)
  private
    FID: string;
    FTitle: string;
    FPlaceholder: PHObjectPlaceholder;
    procedure DoChanges;
  protected
    procedure DoExecute; override;
    procedure DoRequestCompletion; override;
    property ID: string read FID;
  public
    constructor Create(const ATitle: string; const AHandler: TItemChangeRequestCompleteProc);
  end;

  TSaveItemRequest = class(TItemChangeRequest)
  private
    FCollectionID: string;
    FFileName: string;
    FID: string;
    FPlaceholder: PHObjectPlaceholder;
    procedure DoChanges;
  protected
    procedure DoExecute; override;
    procedure DoRequestCompletion; override;
    property ID: string read FID;
  public
    constructor Create(const AFileName: string; const ACollectionID: string; const AHandler: TItemChangeRequestCompleteProc);
  end;

  TDeleteItemRequest = class(TItemChangeRequest)
  private
    FItem: TMediaItem;
    procedure DoChanges;
  protected
    procedure DoExecute; override;
  public
    constructor Create(const AItem: TMediaItem; const AHandler: TItemChangeRequestCompleteProc);
  end;

  TDeleteItemsRequest = class(TItemChangeRequest)
  private
    FIDs: TArray<string>;
    procedure DoChanges;
  protected
    procedure DoExecute; override;
  public
    constructor Create(const AItems: TArray<TMediaItem>; const AHandler: TItemChangeRequestCompleteProc);
  end;

  TUpdateItemRequest = class(TItemChangeRequest)
  private
    FItem: TMediaItem;
    FUpdateFlags: TItemUpdateFlags;
    procedure DoChanges;
  protected
    procedure DoExecute; override;
  public
    constructor Create(const AItem: TMediaItem; const AHandler: TItemChangeRequestCompleteProc);
  end;

  TPlatformMediaCollection = class(TMediaCollection)
  private
    FFetchOptions: TFetchOptions;
    function GetAssetCollection: PHAssetCollection;
  protected
    procedure DeleteItem(const AItem: TMediaItem; const AResultHandler: TDeleteMediaItemResultProc);
    procedure DoChanges(const AChange: PHChange);
    procedure DoLoad; override;
  public
    constructor Create(const AID, ATitle: string); override;
    destructor Destroy; override;
    procedure SaveItem(const AFileName: string; const AResultHandler: TSaveMediaItemResultProc); override;
  end;

  TOpenItem = class(TMediaItem);

function PhotoLibrary: PHPhotoLibrary;
begin
  Result := TPHPhotoLibrary.OCClass.sharedPhotoLibrary;
end;

function ImageManager: PHImageManager;
begin
  Result := TPHImageManager.OCClass.defaultManager;
end;

function GetPredicate(const ACondition: string): NSPredicate;
begin
  Result := TNSPredicate.Wrap(TNSPredicate.OCClass.predicateWithFormat(StrToNSStr(ACondition)));
end;

function GetAssetFromIdentifier(const AID: string): PHAsset;
var
  LResult: PHFetchResult;
begin
  LResult := TPHAsset.OCClass.fetchAssetsWithLocalIdentifiers(StringArrayToNSArray([AID]), nil);
  if LResult <> nil then
    Result := TPHAsset.Wrap(LResult.lastObject);
end;

{ TFetchOptions }

constructor TFetchOptions.Create;
begin
  inherited Create;
  FOptions := TPHFetchOptions.Create;
  FSortDescriptors := TNSMutableArray.Create;
  FOptions.setSortDescriptors(FSortDescriptors);
end;

destructor TFetchOptions.Destroy;
begin
  FOptions.release;
  FOptions := nil;
  FSortDescriptors.release;
  FSortDescriptors := nil;
  inherited;
end;

procedure TFetchOptions.SetPredicate(const AValue: string);
begin
  FPredicate := AValue;
  if FPredicate = '' then
    FOptions.setPredicate(nil)
  else
    FOptions.setPredicate(GetPredicate(AValue));
end;

procedure TFetchOptions.AddSortDescriptor(const AKey: string; const AAscending: Boolean);
var
  LDescriptor: NSSortDescriptor;
begin
  LDescriptor := TNSSortDescriptor.Wrap(TNSSortDescriptor.OCClass.sortDescriptorWithKey(StrToNSStr(AKey), AAscending));
  FSortDescriptors.addObject(NSObjectToID(LDescriptor));
end;

procedure TFetchOptions.Clear;
begin
  FSortDescriptors.removeAllObjects;
  Predicate := '';
end;

{ TLoadItemRequest }

constructor TLoadItemRequest.Create(const AItem: TPlatformMediaItem; const AWidth, AHeight: Single);
begin
  inherited Create;
  FItem := AItem;
  FRequestedWidth := AWidth;
  FRequestedHeight := AHeight;
end;

procedure TLoadItemRequest.LoadItem(const AHandler: TItemLoadCompleteProc);
var
  LNativeAsset: PHAsset;
  // LOptions: PHContentEditingInputRequestOptions;
  // LSize: CGSize;
begin
  FCompleteHandler := AHandler;
  LNativeAsset := GetAssetFromIdentifier(FItem.ID);
  if LNativeAsset <> nil then
  begin
    //!!!! Passing a non-nil PHContentEditingInputRequestOptions causes a crash for me
    // LOptions := TPHContentEditingInputRequestOptions.Create;
    // LAsset.requestContentEditingInputWithOptions(nil, ContentEditingInputRequest);
    RequestImage(LNativeAsset);
  end;
end;

procedure TLoadItemRequest.ContentEditingInputRequest(contentEditingInput: PHContentEditingInput; info: NSDictionary);
var
  LNativeAsset: PHAsset;
begin
  LNativeAsset := GetAssetFromIdentifier(FItem.ID);
  if LNativeAsset <> nil then
    RequestImage(LNativeAsset);
end;

procedure TLoadItemRequest.ImageDataForMediaItemResult(imageData: NSData; dataUTI: NSString; orientation: UIImageOrientation; info: NSDictionary);
var
  LDegradedValue: Pointer;
begin
  LDegradedValue := NSDictionary(info).valueForKey(PHImageResultIsDegradedKey);
  if (LDegradedValue <> nil) and not TNSNumber.Wrap(LDegradedValue).boolValue then
    ImageForMediaItemResult(TUIImage.Wrap(TUIImage.OCClass.imageWithData(imageData)), info);
end;

procedure TLoadItemRequest.ImageForMediaItemResult(image: UIImage; info: NSDictionary);
begin
  FData := TNSData.Wrap(UIImageJPEGRepresentation(NSObjectToID(image), 1));
  FCompleteHandler(Self);
end;

procedure TLoadItemRequest.RequestImage(const ANativeAsset: PHAsset);
var
  LSize: CGSize;
begin
  if (FRequestedWidth > 0) and (FRequestedHeight > 0) then
  begin
    LSize.height := FRequestedHeight;
    LSize.width := FRequestedWidth;
    ImageManager.requestImageForAsset(ANativeAsset, LSize, PHImageContentModeAspectFill, FItem.Options, ImageForMediaItemResult);
  end
  else
    ImageManager.requestImageDataForAsset(ANativeAsset, FItem.Options, ImageDataForMediaItemResult);
end;

{ TPlatformMediaItem }

constructor TPlatformMediaItem.Create(const ACollection: TPlatformMediaCollection; const ANativeAsset: PHAsset);
var
  LFileName: string;
begin
  // https://medium.com/@slk11075/traps-for-phasset-how-to-get-filename-from-phasset-67d856e75c64
  LFileName := NSStrToStr(TNSString.Wrap(ANativeAsset.valueForKey(StrToNSStr('filename'))));
  inherited Create(NSStrToStr(ANativeAsset.localIdentifier), LFileName);
  FCollection := ACollection;
  FOptions := TPHImageRequestOptions.Create;
  DateTime := NSDateToDateTime(ANativeAsset.creationDate, True);
  if ANativeAsset.location <> nil then
    Location := TLocationCoord2D.Create(ANativeAsset.location.coordinate.latitude, ANativeAsset.location.coordinate.longitude);
  Hidden := ANativeAsset.isHidden;
  Favorite := ANativeAsset.isFavorite;
  UpdateFlags := [];
end;

destructor TPlatformMediaItem.Destroy;
begin
  FOptions.release;
  FOptions := nil;
  inherited;
end;

function TPlatformMediaItem.GetOriginalFileName: string;
var
  LNativeAsset: PHAsset;
  LResources: NSArray;
begin
  Result := '';
  LNativeAsset := GetAssetFromIdentifier(ID);
  if LNativeAsset <> nil then
  begin
    LResources := TPHAssetResource.OCClass.assetResourcesForAsset(LNativeAsset);
    if LResources.count > 0 then
      Result := NSStrToStr(TPHAssetResource.Wrap(LResources.objectAtIndex(0)).originalFilename);
  end;
end;

procedure TPlatformMediaItem.Load(const ALoadedHandler: TProc; const AWidth: Single = 0; const AHeight: Single = 0);
var
  LRequest: TLoadItemRequest;
begin
  LRequest := TLoadItemRequest.Create(Self, AWidth, AHeight);
  LRequest.LoadItem(
    procedure(ARequest: TLoadItemRequest)
    begin
      ImageStream.Clear;
      ImageStream.Write(ARequest.Data.bytes^, ARequest.Data.length);
      TThread.Queue(nil, procedure begin ALoadedHandler end);
      ARequest.Free;
    end
  );
end;

procedure TPlatformMediaItem.Delete(const AResultHandler: TDeleteMediaItemResultProc);
begin
  FCollection.DeleteItem(Self, AResultHandler);
end;

procedure TPlatformMediaItem.OptionsChanged;
begin
  inherited;
  //!!!! Check docs for this option
  FOptions.setSynchronous(False);
end;

procedure TPlatformMediaItem.UpdateProperties(const AResultHandler: TUpdateMediaItemResultProc);
begin
  TUpdateItemRequest.Create(Self,
    procedure(ARequest: TItemChangeRequest)
    begin
      if ARequest.Error.IsEmpty then
        UpdateFlags := [];
      AResultHandler(ARequest.Error);
      ARequest.Free;
    end
  ).Execute;
end;

{ TItemChangeRequest }

constructor TItemChangeRequest.Create(const AHandler: TItemChangeRequestCompleteProc);
begin
  inherited Create;
  FCompleteHandler := AHandler;
end;

procedure TItemChangeRequest.PerformChanges(const AChangesProc: TProc);
begin
  // Now for some magic - the performChanges method takes an Objective-C "block": a method that has certain information attached to it
  // For further into, please refer to the DW.Macapi.ObjCBlocks unit
  FDoChangesBlock := TObjCBlock.CreateBlock(AChangesProc);
  PhotoLibrary.performChanges(FDoChangesBlock, RequestCompletionHandler);
end;

procedure TItemChangeRequest.Execute;
begin
  TThread.CreateAnonymousThread(DoExecute).Start;
end;

procedure TItemChangeRequest.RequestCompletionHandler(success: Boolean; error: NSError);
begin
  if success then
    DoRequestCompletion
  else
    FError := NSStrToStr(error.localizedDescription);
  TThread.Synchronize(nil, DoRequestComplete);
end;

procedure TItemChangeRequest.DoRequestComplete;
begin
  FCompleteHandler(Self);
end;

procedure TItemChangeRequest.DoRequestCompletion;
begin
  //
end;

{ TNewCollectionRequest }

constructor TNewCollectionRequest.Create(const ATitle: string; const AHandler: TItemChangeRequestCompleteProc);
begin
  inherited Create(AHandler);
  FTitle := ATitle;
end;

procedure TNewCollectionRequest.DoChanges;
var
  LChangeRequest: Pointer;
begin
  LChangeRequest := TPHAssetCollectionChangeRequest.OCClass.creationRequestForAssetCollectionWithTitle(StrToNSStr(FTitle));
  FPlaceholder := TPHAssetCollectionChangeRequest.Wrap(LChangeRequest).placeholderForCreatedAssetCollection;
  FPlaceholder.retain;
end;

procedure TNewCollectionRequest.DoExecute;
begin
  PerformChanges(DoChanges);
end;

procedure TNewCollectionRequest.DoRequestCompletion;
begin
  FID := NSStrToStr(FPlaceholder.localIdentifier);
  FPlaceholder.release;
end;

{ TSaveItemRequest }

constructor TSaveItemRequest.Create(const AFileName: string; const ACollectionID: string; const AHandler: TItemChangeRequestCompleteProc);
begin
  inherited Create(AHandler);
  FFileName := AFileName;
  FCollectionID := ACollectionID;
end;

procedure TSaveItemRequest.DoChanges;
var
  LImageChangeRequest, LCollectionChangeRequest: Pointer;
  LFetchResult: PHFetchResult;
  LCollection: PHAssetCollection;
begin
  // Find the collection
  LFetchResult := TPHAssetCollection.OCClass.fetchAssetCollectionsWithLocalIdentifiers(StringArrayToNSArray([FCollectionID]), nil);
  if LFetchResult.count > 0 then
  begin
    LCollection := TPHAssetCollection.Wrap(LFetchResult.objectAtIndex(0));
    // Create a request for the asset
    LImageChangeRequest := TPHAssetChangeRequest.OCClass.creationRequestForAssetFromImageAtFileURL(StrToNSUrl(FFileName));
    // Obtain the asset placeholder
    FPlaceholder := TPHAssetChangeRequest.Wrap(LImageChangeRequest).placeholderForCreatedAsset;
    FPlaceholder.retain;
    // Create change request for the collection
    LCollectionChangeRequest := TPHAssetCollectionChangeRequest.OCClass.changeRequestForAssetCollection(LCollection);
    // Add asset placeholder to the collection change request
    TPHAssetCollectionChangeRequest.Wrap(LCollectionChangeRequest).addAssets(TNSArray.OCClass.arrayWithObject(NSObjectToID(FPlaceholder)));
  end;
end;

procedure TSaveItemRequest.DoExecute;
begin
  PerformChanges(DoChanges);
end;

procedure TSaveItemRequest.DoRequestCompletion;
begin
  if FPlaceholder <> nil then
  begin
    FID := NSStrToStr(FPlaceholder.localIdentifier);
    FPlaceholder.release;
  end;
end;

{ TDeleteItemRequest }

constructor TDeleteItemRequest.Create(const AItem: TMediaItem; const AHandler: TItemChangeRequestCompleteProc);
begin
  inherited Create(AHandler);
  FItem := AItem;
end;

procedure TDeleteItemRequest.DoChanges;
var
  LNativeAsset: PHAsset;
begin
  LNativeAsset := GetAssetFromIdentifier(FItem.ID);
  TPHAssetChangeRequest.OCClass.deleteAssets(TNSArray.OCClass.arrayWithObject(NSObjectToID(LNativeAsset)));
end;

procedure TDeleteItemRequest.DoExecute;
begin
  PerformChanges(DoChanges);
end;

{ TDeleteItemsRequest }

constructor TDeleteItemsRequest.Create(const AItems: TArray<TMediaItem>; const AHandler: TItemChangeRequestCompleteProc);
var
  LItem: TMediaItem;
begin
  inherited Create(AHandler);
  for LItem in AItems do
    FIDs := FIDs + [LItem.ID];
end;

procedure TDeleteItemsRequest.DoChanges;
var
  LNativeAsset: PHAsset;
  LID: string;
  LArray: NSMutableArray;
begin
  LArray := TNSMutableArray.Create;
  for LID in FIDs do
  begin
    LNativeAsset := GetAssetFromIdentifier(LID);
    if LNativeAsset <> nil then
      LArray.addObject(NSObjectToID(LNativeAsset));
  end;
  TPHAssetChangeRequest.OCClass.deleteAssets(NSObjectToID(LArray));
end;

procedure TDeleteItemsRequest.DoExecute;
begin
  PerformChanges(DoChanges);
end;

{ TUpdateItemRequest }

constructor TUpdateItemRequest.Create(const AItem: TMediaItem; const AHandler: TItemChangeRequestCompleteProc);
begin
  inherited Create(AHandler);
  FItem := AItem;
end;

procedure TUpdateItemRequest.DoChanges;
var
  LUpdateFlags: TItemUpdateFlags;
  LNativeAsset: PHAsset;
  LChangeRequest: PHAssetChangeRequest;
  LLocation: CLLocation;
begin
  LUpdateFlags := TOpenItem(FItem).UpdateFlags;
  LNativeAsset := GetAssetFromIdentifier(FItem.ID);
  LChangeRequest := TPHAssetChangeRequest.Wrap(TPHAssetChangeRequest.OCClass.changeRequestForAsset(LNativeAsset));
  if TItemUpdateFlag.DateTime in LUpdateFlags then
    LChangeRequest.setCreationDate(DateTimeToNSDate(FItem.DateTime, True));
  if TItemUpdateFlag.Location in LUpdateFlags then
  begin
    LLocation := TCLLocation.Create;
    LLocation := TCLLocation.Wrap(LLocation.initWithLatitude(FItem.Location.Latitude, FItem.Location.Longitude));
    LChangeRequest.setLocation(LLocation);
  end;
  if TItemUpdateFlag.Favorite in LUpdateFlags then
    LChangeRequest.setFavorite(FItem.Favorite);
  if TItemUpdateFlag.Hidden in LUpdateFlags then
    LChangeRequest.setHidden(FItem.Hidden);
end;

procedure TUpdateItemRequest.DoExecute;
begin
  PerformChanges(DoChanges);
end;

{ TPlatformMediaCollection }

constructor TPlatformMediaCollection.Create(const AID, ATitle: string);
begin
  inherited;
  FFetchOptions := TFetchOptions.Create;
end;

destructor TPlatformMediaCollection.Destroy;
begin
  FFetchOptions.Free;
  inherited;
end;

function TPlatformMediaCollection.GetAssetCollection: PHAssetCollection;
var
  LFetchResult: PHFetchResult;
begin
  Result := nil;
  LFetchResult := TPHAssetCollection.OCClass.fetchAssetCollectionsWithLocalIdentifiers(StringArrayToNSArray([ID]), nil);
  if LFetchResult.count > 0 then
    Result := TPHAssetCollection.Wrap(LFetchResult.objectAtIndex(0));
end;

procedure TPlatformMediaCollection.DoChanges(const AChange: PHChange);
//var
//  LAssetCollection: PHAssetCollection;
//  LChangeDetails: PHObjectChangeDetails;
begin
{
  LAssetCollection := GetAssetCollection;
  if LAssetCollection <> nil then
  begin
    LChangeDetails := AChange.changeDetailsForObject(LAssetCollection);
    if LChangeDetails <> nil then
    begin
      // This is a to-do
      // Use LChangeDetails.objectWasDeleted flag to remove deleted album(s)
    end;
  end;
}
end;

procedure TPlatformMediaCollection.DoLoad;
var
  LCollection: PHAssetCollection;
  LFetchResult: PHFetchResult;
  I: Integer;
  LNativeAsset: PHAsset;
begin
  LCollection := GetAssetCollection;
  if LCollection <> nil then
  begin
    FFetchOptions.Clear;
    FFetchOptions.AddSortDescriptor('creationDate', False); //  CreationDateSort = TPhotoSortType.Ascending);
    LFetchResult := TPHAsset.OCClass.fetchAssetsInAssetCollection(LCollection, FFetchOptions.Options);
    if LFetchResult <> nil then
    begin
      for I := 0 to LFetchResult.count - 1 do
      begin
        LNativeAsset := TPHAsset.Wrap(LFetchResult.objectAtIndex(I));
        if LNativeAsset <> nil then
          Add(TPlatformMediaItem.Create(Self, LNativeAsset));
      end;
    end;
  end;
end;

procedure TPlatformMediaCollection.DeleteItem(const AItem: TMediaItem; const AResultHandler: TDeleteMediaItemResultProc);
begin
  TDeleteItemRequest.Create(AItem,
    procedure(ARequest: TItemChangeRequest)
    begin
      if ARequest.Error.IsEmpty then
        Remove(AItem);
      AResultHandler(ARequest.Error);
      ARequest.Free;
    end
  ).Execute;
end;

procedure TPlatformMediaCollection.SaveItem(const AFileName: string; const AResultHandler: TSaveMediaItemResultProc);
begin
  TSaveItemRequest.Create(AFileName, ID,
    procedure(ARequest: TItemChangeRequest)
    var
      LItem: TMediaItem;
      LChangeRequest: TSaveItemRequest;
    begin
      LItem := nil;
      LChangeRequest := TSaveItemRequest(ARequest);
      if not LChangeRequest.ID.IsEmpty then
      begin
        LItem := TPlatformMediaItem.Create(Self, GetAssetFromIdentifier(LChangeRequest.ID));
        Add(LItem);
      end;
      AResultHandler(LItem, ARequest.Error);
      ARequest.Free;
    end
  ).Execute;
end;

{ TPlatformMediaCollections }

constructor TPlatformMediaCollections.Create;
begin
  inherited;
  CollectionTypes := [PHAssetCollectionTypeAlbum, PHAssetCollectionTypeSmartAlbum];
  CollectionSubtypes := [PHAssetCollectionSubtypeAlbumRegular, PHAssetCollectionSubtypeSmartAlbumUserLibrary,
    PHAssetCollectionSubtypeSmartAlbumFavorites];
  FFetchOptions := TFetchOptions.Create;
end;

destructor TPlatformMediaCollections.Destroy;
begin
  FFetchOptions.Free;
  inherited;
end;

procedure TPlatformMediaCollections.DoChanges(const AChange: PHChange);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TPlatformMediaCollection(Items[I]).DoChanges(AChange);
end;

function TPlatformMediaCollections.InternalCollectionTitleExists(const AAlbumType: PHAssetCollectionType; const ATitle: string): Boolean;
var
  LFetchResult: PHFetchResult;
  I: Integer;
begin
  Result := False;
  LFetchResult := TPHAssetCollection.OCClass.fetchAssetCollectionsWithType(AAlbumType, PHAssetCollectionSubtypeAny, nil);
  if LFetchResult <> nil then
  begin
    for I := 0 to LFetchResult.count - 1 do
    begin
      if NSStrToStr(TPHAssetCollection.Wrap(LFetchResult.objectAtIndex(I)).localizedTitle).Equals(ATitle) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TPlatformMediaCollections.CollectionTitleExists(const ATitle: string): Boolean;
begin
  Result := InternalCollectionTitleExists(PHAssetCollectionTypeSmartAlbum, ATitle) or
    InternalCollectionTitleExists(PHAssetCollectionTypeAlbum, ATitle);
end;

procedure TPlatformMediaCollections.FetchAlbums(const AAlbumType: PHAssetCollectionType; const AAlbumSubtype: PHAssetCollectionSubtype);
var
  LFetchResult: PHFetchResult;
  LAssetCollection: PHAssetCollection;
  I: Integer;
  LID, LTitle: string;
begin
  LFetchResult := TPHAssetCollection.OCClass.fetchAssetCollectionsWithType(AAlbumType, AAlbumSubtype, FFetchOptions.Options);
  if LFetchResult <> nil then
  begin
    for I := 0 to LFetchResult.count - 1 do
    begin
      LAssetCollection := TPHAssetCollection.Wrap(LFetchResult.objectAtIndex(I));
      LID := NSStrToStr(LAssetCollection.localIdentifier);
      LTitle := NSStrToStr(LAssetCollection.localizedTitle);
      if (IndexOfID(LID) = -1) and (LAssetCollection.assetCollectionType = AAlbumType) and (LAssetCollection.AssetCollectionSubtype = AAlbumSubtype) then
        Add(TPlatformMediaCollection.Create(LID, LTitle));
    end;
  end;
end;

procedure TPlatformMediaCollections.DoLoad;
var
  LAlbumType: PHAssetCollectionType;
  LAlbumSubtype: PHAssetCollectionSubtype;
begin
  for LAlbumType in CollectionTypes do
  begin
    for LAlbumSubtype in CollectionSubtypes do
      FetchAlbums(LAlbumType, LAlbumSubtype);
  end;
end;

procedure TPlatformMediaCollections.NewCollection(const ATitle: string; const AResultHandler: TNewCollectionResultProc);
begin
  TNewCollectionRequest.Create(ATitle,
    procedure(ARequest: TItemChangeRequest)
    var
      LCollection: TMediaCollection;
      LChangeRequest: TNewCollectionRequest;
    begin
      LCollection := nil;
      LChangeRequest := TNewCollectionRequest(ARequest);
      if not LChangeRequest.ID.IsEmpty then
      begin
        LCollection := TPlatformMediaCollection.Create(LChangeRequest.ID, ATitle);
        Add(LCollection);
      end;
      AResultHandler(LCollection, ARequest.Error);
      ARequest.Free;
    end
  ).Execute;
end;

{ TChangeObserver }

constructor TChangeObserver.Create(const AMediaManager: TPlatformMediaManager);
begin
  inherited Create;
  FMediaManager := AMediaManager;
end;

procedure TChangeObserver.photoLibraryDidChange(changeInstance: PHChange);
begin
  FMediaManager.DoChanges(changeInstance);
end;

{ TPlatformMediaManager }

constructor TPlatformMediaManager.Create(const AMediaManager: TMediaManager);
begin
  inherited;
  //
end;

procedure TPlatformMediaManager.DeleteItems(const AItems: TArray<TMediaItem>; const AResultHandler: TDeleteMediaItemResultProc);
begin
  TDeleteItemsRequest.Create(AItems,
    procedure(ARequest: TItemChangeRequest)
    begin
      AResultHandler(ARequest.Error);
      ARequest.Free;
    end
  ).Execute;
end;

destructor TPlatformMediaManager.Destroy;
begin
  if FChangeObserver <> nil then
  begin
    PhotoLibrary.unregisterChangeObserver(FChangeObserver.GetObjectID);
    FChangeObserver.Free;
  end;
  inherited;
end;

procedure TPlatformMediaManager.DoChanges(const AChange: PHChange);
begin
  TPlatformMediaCollections(Collections).DoChanges(AChange);
end;

procedure TPlatformMediaManager.RequestPermission;
begin
  TPHPhotoLibrary.OCClass.requestAuthorization(RequestAuthorizationHandler);
end;

procedure TPlatformMediaManager.RequestAuthorizationHandler(AStatus: PHAuthorizationStatus);
begin
  if GetAuthorizationStatus = TAuthorizationStatus.Authorized then
  begin
    FChangeObserver := TChangeObserver.Create(Self);
    PhotoLibrary.registerChangeObserver(FChangeObserver.GetObjectID);
  end;
  TThread.Queue(nil, DoAuthorizationStatus);
end;

function TPlatformMediaManager.GetAuthorizationStatus: TAuthorizationStatus;
begin
  case TPHPhotoLibrary.OCClass.authorizationStatus of
    PHAuthorizationStatusDenied:
      Result := TAuthorizationStatus.Denied;
    PHAuthorizationStatusRestricted:
      Result := TAuthorizationStatus.Restricted;
    PHAuthorizationStatusAuthorized:
      Result := TAuthorizationStatus.Authorized;
  else
    Result := TAuthorizationStatus.NotDetermined;
  end;
end;

end.
