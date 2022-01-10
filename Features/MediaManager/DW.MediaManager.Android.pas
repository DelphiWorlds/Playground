unit DW.MediaManager.Android;

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
  System.Messaging, System.Classes,
  // Android
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Net, Androidapi.JNIBridge,
  Androidapi.JNI.Media, Androidapi.JNI.Embarcadero, Androidapi.JNI.Location,
  Androidapi.JNI.Os,
  // DW
  DW.MediaManager, DW.Types;

type
  TScanCompletedEvent = procedure(Sender: TObject; const Path: JString; const Uri: Jnet_Uri) of object;

  TMediaScanner = class(TJavaLocal, JMediaScannerConnection_MediaScannerConnectionClient)
  private
    FConnection: JMediaScannerConnection;
    FFilePath: string;
    FTempFilePath: string;
    FOnCompleted: TScanCompletedEvent;
  public
    { JMediaScannerConnection_MediaScannerConnectionClient }
    procedure onMediaScannerConnected; cdecl;
    procedure onScanCompleted(path: JString; uri: Jnet_Uri); cdecl;
  public
    procedure ScanFile(const AFilePath: string);
    property OnCompleted: TScanCompletedEvent read FOnCompleted write FOnCompleted;
  end;

  // This might look a little weird - JOnAddressBookChangesListener is actually generic to observing content changes
  TAlbumsContentChangesListener = class(TJavaLocal, JOnAddressBookChangesListener)
  private
    FContentObserver: JAddressBookObserver;
    FIncludeSelf: Boolean;
    FOnContentChanged: TNotifyEvent;
  public
    { JOnAddressBookChangesListener }
    procedure onChanged(selfChange: Boolean); cdecl;
  public
    constructor Create;
    destructor Destroy; override;
    property IncludeSelf: Boolean read FIncludeSelf write FIncludeSelf;
    property OnContentChanged: TNotifyEvent read FOnContentChanged write FOnContentChanged;
  end;

  TPlatformMediaManager = class(TCustomPlatformMediaManager)
  private
    FAuthorizationStatus: TAuthorizationStatus;
    FContentChangesListener: TAlbumsContentChangesListener;
    FMediaScanner: TMediaScanner;
    procedure ContentChangesListenerContentChangeHandler(Sender: TObject);
    procedure MediaScannerCompletedHandler(Sender: TObject; const APath: JString; const AUri: Jnet_Uri);
  protected
    procedure DeleteItems(const AItems: TArray<TMediaItem>; const AResultHandler: TDeleteMediaItemResultProc); override;
    function GetAuthorizationStatus: TAuthorizationStatus; override;
    function GetCameraRoll: TMediaCollection; override;
    procedure ReceivedImagePathMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure RequestPermission; override;
  public
    constructor Create(const AMediaManager: TMediaManager); override;
    destructor Destroy; override;
  end;

  TPlatformMediaCollections = class(TMediaCollections)
  protected
    procedure DoLoad; override;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Types, System.Threading, System.Permissions,
  System.IOUtils,
  // Android
  Androidapi.JNI.Provider, Androidapi.Helpers,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI,
  // FMX
  FMX.Helpers.Android, FMX.Platform, FMX.Graphics, FMX.Platform.Android,
  // DW
  { DW.Graphics.Helpers.Android, } DW.OSLog, DW.Consts.Android,
  DW.Permissions.Helpers;

type
  TPlatformMediaCollection = class;

  TPlatformMediaItem = class(TMediaItem)
  private
    const
      cThumbnailSize: TPoint = (X: 96; Y: 96);
  private
    FCollection: TPlatformMediaCollection;
    // FMediaType: string;
    FMimeType: string;
    // procedure ExtractEXIF(const APath: string);
  protected
    function InternalDelete: Boolean;
    function GetURI: Jnet_Uri;
  public
    constructor Create(const ACollection: TPlatformMediaCollection; const AID, AFileName: string); reintroduce;
    procedure Delete(const AResultHandler: TDeleteMediaItemResultProc); override;
    procedure Load(const ALoadedHandler: TProc; const AWidth: Single = 0; const AHeight: Single = 0); override;
    procedure UpdateProperties(const AResultHandler: TUpdateMediaItemResultProc); override;
  end;

  TPlatformMediaCollection = class(TMediaCollection)
  private
    procedure AddItem(const AID: Integer; const APath, AMimeType{, AMediaType}: string);
    procedure DeleteItem(const AItem: TPlatformMediaItem; const AResultHandler: TDeleteMediaItemResultProc);
    function GetCursor: JCursor;
  protected
    procedure DoLoad; override;
  public
    constructor Create(const AID, ATitle: string); override;
    destructor Destroy; override;
  end;

{ TPlatformMediaItem }

(*
procedure TPlatformMediaItem.ExtractEXIF(const APath: string);
var
  LEXIF: JExifInterface;
  LLatLong: TJavaArray<Single>;
  LOrientation: Integer;
begin
  LEXIF := TJExifInterface.JavaClass.init(StringToJString(APath));
  FProperties.DateTaken := JStringToString(LEXIF.getAttribute(TJExifInterface.JavaClass.TAG_DATETIME));
  FProperties.CameraMake := JStringToString(LEXIF.getAttribute(TJExifInterface.JavaClass.TAG_MAKE));
  FProperties.CameraModel := JStringToString(LEXIF.getAttribute(TJExifInterface.JavaClass.TAG_MODEL));
  FProperties.Orientation := TPhotoPropertyOrientation.Unknown;
  LOrientation := StrToIntDef(JStringToString(LEXIF.getAttribute(TJExifInterface.JavaClass.TAG_ORIENTATION)), -1);
  if LOrientation = TJExifInterface.JavaClass.ORIENTATION_NORMAL then
    FProperties.Orientation := TPhotoPropertyOrientation.Normal
  else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_FLIP_HORIZONTAL then
    FProperties.Orientation := TPhotoPropertyOrientation.FlipHorizontal
  else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_ROTATE_180 then
    FProperties.Orientation := TPhotoPropertyOrientation.Rotate180
  else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_FLIP_VERTICAL then
    FProperties.Orientation := TPhotoPropertyOrientation.FlipVertical
  else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_TRANSPOSE then
    FProperties.Orientation := TPhotoPropertyOrientation.Transpose
  else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_ROTATE_90 then
    FProperties.Orientation := TPhotoPropertyOrientation.Rotate90
  else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_TRANSVERSE then
    FProperties.Orientation := TPhotoPropertyOrientation.Transverse
  else if LOrientation = TJExifInterface.JavaClass.ORIENTATION_ROTATE_270 then
    FProperties.Orientation := TPhotoPropertyOrientation.Rotate270;
  LLatLong := TJavaArray<Single>.Create(2);
  try
    if LEXIF.getLatLong(LLatLong) then
    begin
      FProperties.Latitude := LLatLong.Items[0];
      FProperties.Longitude := LLatLong.Items[1];
    end;
  finally
    LLatLong.Free;
  end;
  FProperties.HasProperties := True;
end;
*)

constructor TPlatformMediaItem.Create(const ACollection: TPlatformMediaCollection; const AID, AFileName: string);
begin
  inherited Create(AID, AFileName);
  FCollection := ACollection;
  // FFileNameShort := TPath.GetFileName(AFileName);
end;

procedure TPlatformMediaItem.Load(const ALoadedHandler: TProc; const AWidth, AHeight: Single);
var
  LUri: Jnet_Uri;
  LBitmap: JBitmap;
  LInputStream: JInputStream;
  LOutputStream: JByteArrayOutputStream;
  LData: TBytes;
begin
  TOSLog.d('Loading: %s', [FileName]);
  // ExtractEXIF(FileName);
  TOSLog.d('Extracted EXIF');
  LUri := TAndroidHelper.JFileToJURI(TJFile.JavaClass.init(StringToJString(FileName)));
  TAndroidHelper.Context.grantUriPermission(TAndroidHelper.Context.getPackageName, LUri, TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION or TJIntent.JavaClass.FLAG_GRANT_WRITE_URI_PERMISSION);
  TOSLog.d('Called grantUriPermission');
  LInputStream := TAndroidHelper.ContentResolver.openInputStream(LUri);
  TOSLog.d('Opened LInputStream');
  // LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(FFileName));
  LBitmap := TJBitmapFactory.JavaClass.decodeStream(LInputStream);
  LOutputStream := TJByteArrayOutputStream.Create;
  LBitmap.compress(TJBitmap_CompressFormat.JavaClass.JPEG, 100, LOutputStream);
  LData := TAndroidHelper.TJavaArrayToTBytes(LOutputStream.toByteArray);
  ImageStream.Size := 0;
  ImageStream.Write(LData, Length(LData));
  LBitmap.recycle;
  if Assigned(ALoadedHandler) then
    ALoadedHandler;
end;

// https://stackoverflow.com/a/53017713/3164070
procedure TPlatformMediaItem.Delete(const AResultHandler: TDeleteMediaItemResultProc);
begin
  FCollection.DeleteItem(Self, AResultHandler);
end;

function TPlatformMediaItem.GetURI: Jnet_Uri;
begin
  Result := TJContentUris.JavaClass.withAppendedId(TJImages_Media.JavaClass.EXTERNAL_CONTENT_URI, StrToInt(ID));
end;

function TPlatformMediaItem.InternalDelete: Boolean;
begin
  Result := TAndroidHelper.ContentResolver.delete(GetURI, nil, nil) = 1;
end;

procedure TPlatformMediaItem.UpdateProperties(const AResultHandler: TUpdateMediaItemResultProc);
begin
  // TODO
end;

{ TPlatformMediaCollection }

constructor TPlatformMediaCollection.Create(const AID, ATitle: string);
begin
  inherited;
  //
end;

procedure TPlatformMediaCollection.DeleteItem(const AItem: TPlatformMediaItem; const AResultHandler: TDeleteMediaItemResultProc);
begin
  if AItem.InternalDelete then
  begin
    Remove(AItem);
    AItem.Free;
    AResultHandler('');
  end
  else
    AResultHandler(Format('Failed to delete asset with id: %s', [ID]));
end;

destructor TPlatformMediaCollection.Destroy;
begin
  //
  inherited;
end;

procedure TPlatformMediaCollection.AddItem(const AID: Integer; const APath, AMimeType{, AMediaType}: string);
var
  LItem: TPlatformMediaItem;
begin
  LItem := TPlatformMediaItem.Create(Self, AID.ToString, APath);
  // LAsset.FMediaType := AMediaType;
  LItem.FMimeType := AMimeType;
  Add(LItem);
end;

procedure TPlatformMediaCollection.DoLoad;
var
  LCursor: JCursor;
  LPath, {LMediaType, } LMimeType: string;
  LID: Integer;
  LIDField: JString;
begin
  inherited;
  LIDField := StringToJString('_id'); // TJBaseColumns.JavaClass._ID;
  // TOSLog.d('TPlatformMediaCollection.DoReload ID field is %s', [JStringToString(LIDField)]);
  LCursor := GetCursor;
  if (LCursor <> nil) and LCursor.moveToFirst then
  begin
    repeat
      LID := LCursor.getInt(LCursor.getColumnIndex(LIDField));
      LPath := JStringToString(LCursor.getString(LCursor.getColumnIndex(TJMediaStore_MediaColumns.JavaClass.DATA)));
      LMimeType := JStringToString(LCursor.getString(LCursor.getColumnIndex(TJMediaStore_MediaColumns.JavaClass.MIME_TYPE)));
      // LMediaType := JStringToString(LCursor.getString(LCursor.getColumnIndex(TJFiles_FileColumns.JavaClass.MEDIA_TYPE)));
      AddItem(LID, LPath, LMimeType); // , LMediaType);
    until not LCursor.moveToNext;
  end;
end;

function TPlatformMediaCollection.GetCursor: JCursor;
var
  LBaseURI: Jnet_Uri;
  LProjection, LSelectionArgs: TJavaObjectArray<JString>;
  LSelection, LOrderBy: JString;
begin
  LBaseURI := TJImages_Media.JavaClass.EXTERNAL_CONTENT_URI;
  LProjection := TJavaObjectArray<JString>.Create(3);
  try
    LProjection[0] := StringToJString('_id'); // TJBaseColumns.JavaClass._ID; // TJImages_ImageColumns.JavaClass._ID;
    // TOSLog.d('TPlatformMediaCollection.GetCursor ID field is %s', [JStringToString(LProjection[0])]);
    LProjection[1] := TJMediaStore_MediaColumns.JavaClass.DATA;
    LProjection[2] := TJMediaStore_MediaColumns.JavaClass.MIME_TYPE;
    // LProjection[3] := TJFiles_FileColumns.JavaClass.MEDIA_TYPE;
    LOrderBy := StringToJString('date_added DESC'); //  TJImages_ImageColumns.JavaClass.DATE_ADDED.concat(StringToJString(' DESC'));
    // LSelection := TJImages_ImageColumns.JavaClass.BUCKET_ID.concat(StringToJString(' = ?'));
    LSelection := StringToJString('bucket_id = ?');
    LSelectionArgs := TJavaObjectArray<JString>.Create(1);
    try
      LSelectionArgs[0] := StringToJString(ID);
      Result := TAndroidHelper.ContentResolver.query(LBaseURI, LProjection, LSelection, LSelectionArgs, LOrderBy);
    finally
      LSelectionArgs.Free;
    end;
  finally
    LProjection.Free;
  end;
end;

{ TPlatformMediaCollections }

procedure TPlatformMediaCollections.DoLoad;
var
  LBaseURI: Jnet_Uri;
  LIDField, LOrderBy: JString;
  LProjection: TJavaObjectArray<JString>;
  LCursor: JCursor;
  LTitle, LID: string;
  LCollection: TPlatformMediaCollection;
begin
  LBaseURI := TJImages_Media.JavaClass.EXTERNAL_CONTENT_URI;
  LIDField := StringToJString('bucket_id'); // TJImages_ImageColumns.JavaClass.BUCKET_ID;
  // TOSLog.d('ID field is %s', [JStringToString(LIDField)]);
  LProjection := TJavaObjectArray<JString>.Create(2);
  LProjection[0] := LIDField;
  LProjection[1] := TJImages_ImageColumns.JavaClass.BUCKET_DISPLAY_NAME;
  LOrderBy := TJImages_ImageColumns.JavaClass.BUCKET_DISPLAY_NAME.concat(StringToJString(' ASC'));
  LCursor := TAndroidHelper.ContentResolver.query(LBaseURI, LProjection, nil, nil, LOrderBy);
  if (LCursor <> nil) and LCursor.moveToFirst then
  begin
    repeat
      LID := JStringToString(LCursor.getString(LCursor.getColumnIndex(LIDField)));
      if IndexOfID(LID) = -1 then
      begin
        LTitle := JStringToString(LCursor.getString(LCursor.getColumnIndex(TJImages_ImageColumns.JavaClass.BUCKET_DISPLAY_NAME)));
        LCollection := TPlatformMediaCollection.Create(LID, LTitle);
        TOSLog.d('Adding collection: %s (%s)', [LTitle, LID]);
        Add(LCollection);
      end;
    until not LCursor.moveToNext;
  end;
end;

{ TMediaScanner }

procedure TMediaScanner.ScanFile(const AFilePath: string);
begin
  // Now for a bit of "trickery" - by the time the mediascanner is connected, FMX has deleted the file, so the file is copied,
  // and when the scanner has connected, it is copied back so that the scan can succeed
  FFilePath := AFilePath;
  FTempFilePath := TPath.Combine(TPath.GetTempPath, TPath.GetFileName(FFilePath));
  TFile.Copy(FFilePath, FTempFilePath);
  if FConnection = nil then
    FConnection := TJMediaScannerConnection.JavaClass.init(TAndroidHelper.Context, Self);
  FConnection.connect;
end;

procedure TMediaScanner.onMediaScannerConnected;
begin
  // Now that that mediascanner is connected, copy the temp file back to the original location
  TFile.Copy(FTempFilePath, FFilePath);
  TFile.Delete(FTempFilePath);
  FConnection.scanFile(StringToJString(FFilePath), nil);
end;

procedure TMediaScanner.onScanCompleted(path: JString; uri: Jnet_Uri);
begin
  FConnection.disconnect;
  if Assigned(FOnCompleted) then
    FOnCompleted(Self, path, uri);
end;

{ TAlbumsContentChangesListener }

constructor TAlbumsContentChangesListener.Create;
begin
  inherited;
  FContentObserver := TJAddressBookObserver.JavaClass.init(Self);
  TAndroidHelper.ContentResolver.registerContentObserver(TJImages_Media.JavaClass.EXTERNAL_CONTENT_URI, True, FContentObserver);
end;

destructor TAlbumsContentChangesListener.Destroy;
begin
  TAndroidHelper.ContentResolver.unregisterContentObserver(FContentObserver);
  inherited;
end;

procedure TAlbumsContentChangesListener.onChanged(selfChange: Boolean);
begin
  if (not selfChange or FIncludeSelf) and Assigned(FOnContentChanged) then
    FOnContentChanged(Self);
end;

{ TPlatformMediaManager }

constructor TPlatformMediaManager.Create(const AMediaManager: TMediaManager);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedImagePath, ReceivedImagePathMessageHandler);
  FMediaScanner := TMediaScanner.Create;
  FMediaScanner.OnCompleted := MediaScannerCompletedHandler;
  FContentChangesListener := TAlbumsContentChangesListener.Create;
  FContentChangesListener.OnContentChanged := ContentChangesListenerContentChangeHandler;
end;

procedure TPlatformMediaManager.DeleteItems(const AItems: TArray<TMediaItem>; const AResultHandler: TDeleteMediaItemResultProc);
var
  LItem: TMediaItem;
  LFailed: TArray<string>;
begin
  // If deleting a large number, might pay to have this done asynch
  LFailed := [];
  for LItem in AItems do
  begin
    if not TPlatformMediaItem(LItem).InternalDelete then
      LFailed := LFailed + [LItem.ID];
  end;
  if Length(LFailed) > 0 then
    AResultHandler(Format('Failed to delete assets with the following ids: %s', [string.Join(', ', LFailed)]))
  else
    AResultHandler('');
end;

destructor TPlatformMediaManager.Destroy;
begin
  FContentChangesListener.Free;
  FMediaScanner.Free;
  TMessageManager.DefaultManager.Unsubscribe(TMessageReceivedImagePath, ReceivedImagePathMessageHandler);
  inherited;
end;

function TPlatformMediaManager.GetAuthorizationStatus: TAuthorizationStatus;
begin
  Result := FAuthorizationStatus;
end;

function TPlatformMediaManager.GetCameraRoll: TMediaCollection;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Collections.Count - 1 do
  begin
    if Collections[I].Title.Equals('Camera') then
      Exit(Collections[I]);
  end;
end;

procedure TPlatformMediaManager.ContentChangesListenerContentChangeHandler(Sender: TObject);
begin
  // DoChanges; // Might need to be synch'd?
end;

procedure TPlatformMediaManager.MediaScannerCompletedHandler(Sender: TObject; const APath: JString; const AUri: Jnet_Uri);
begin
  // TThread.Synchronize(nil, DoChanges);
end;

procedure TPlatformMediaManager.ReceivedImagePathMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  FMediaScanner.ScanFile(TMessageReceivedImagePath(AMsg).Value);
end;

procedure TPlatformMediaManager.RequestPermission;
begin
  PermissionsService.RequestPermissions([cPermissionReadExternalStorage, cPermissionWriteExternalStorage, cPermissionCamera],
    procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
    begin
      if AGrantResults.AreAllGranted then
        FAuthorizationStatus := TAuthorizationStatus.Authorized
      else
        FAuthorizationStatus := TAuthorizationStatus.Denied;
      DoAuthorizationStatus;
    end
  );
end;

end.

