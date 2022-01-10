unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Actions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox,
  FMX.Layouts, FMX.Objects, FMX.TabControl, FMX.ActnList, FMX.StdActns, FMX.MediaLibrary.Actions, FMX.Edit,
  DW.MediaManager, DW.Types, DW.ElasticLayout, DW.AlertView;

type
  TForm1 = class(TForm)
    AlbumsComboBox: TComboBox;
    TabControl: TTabControl;
    MediaTab: TTabItem;
    SaveTab: TTabItem;
    CollectionImage: TImage;
    PhotosListBox: TListBox;
    TakenPhotoImage: TImage;
    SaveImageButtonsOuterLayout: TLayout;
    SaveImageButtonsLayout: TLayout;
    TakePhotoButton: TButton;
    SavePhotoButton: TButton;
    ActionList: TActionList;
    TakePhotoFromCameraAction: TTakePhotoFromCameraAction;
    SavePhotoAction: TAction;
    CollectionsTab: TTabItem;
    CollectionNameEdit: TEdit;
    CollectionButtonsOuterLayout: TLayout;
    CollectionButtonsLayout: TLayout;
    AddCollectionButton: TButton;
    DeleteCollectionButton: TButton;
    AddCollectionAction: TAction;
    DeleteCollectionAction: TAction;
    CollectionNameLabel: TLabel;
    Layout1: TLayout;
    Layout2: TLayout;
    Button1: TButton;
    DeletePhotoAction: TAction;
    Layout3: TLayout;
    Layout4: TLayout;
    Label1: TLabel;
    PhotoCreatedDateLabel: TLabel;
    Label2: TLabel;
    PhotoLocationLabel: TLabel;
    TestChangeButton: TButton;
    TestChangeAction: TAction;
    Label3: TLabel;
    PhotoFavoriteLabel: TLabel;
    Label4: TLabel;
    PhotoHiddenLabel: TLabel;
    procedure PhotosListBoxItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
    procedure AlbumsComboBoxChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TakePhotoFromCameraActionDidFinishTaking(Image: TBitmap);
    procedure TakePhotoButtonClick(Sender: TObject);
    procedure SavePhotoActionUpdate(Sender: TObject);
    procedure SavePhotoActionExecute(Sender: TObject);
    procedure AddCollectionActionUpdate(Sender: TObject);
    procedure DeleteCollectionActionUpdate(Sender: TObject);
    procedure AddCollectionActionExecute(Sender: TObject);
    procedure DeletePhotoActionUpdate(Sender: TObject);
    procedure DeletePhotoActionExecute(Sender: TObject);
    procedure TestChangeActionUpdate(Sender: TObject);
    procedure TestChangeActionExecute(Sender: TObject);
  private
    FAlertView: TAlertView;
    FMediaManager: TMediaManager;
    procedure AlbumReset;
    procedure AlbumSelected(const AReload: Boolean = False);
    procedure DeletePhotoHandler(Sender: TObject);
    procedure DeleteSelectedPhotoListBoxItems;
    procedure LoadAlbumsComboBox(const ASelectID: string = '');
    procedure PhotoLoadedHandler(Sender: TObject);
    procedure PhotoSelected;
    procedure PhotosAuthorizationStatusHandler(Sender: TObject; const AStatus: TAuthorizationStatus);
    procedure UpdatePhotoLabels(const AItem: TMediaItem);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils;

type
  TListBoxHelper = class helper for TListBox
  public
    function SelectedCount: Integer;
  end;

const
  cJPEGExtension = '.jpg';

{ TListBoxHelper }

function TListBoxHelper.SelectedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
  begin
    if ListItems[I].IsSelected then
      Inc(Result);
  end;
end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  SaveImageButtonsLayout.IsElastic := True;
  FAlertView := TAlertView.Create;
  FMediaManager := TMediaManager.Create;
  FMediaManager.OnAuthorizationStatus := PhotosAuthorizationStatusHandler;
  FMediaManager.RequestPermission;
end;

destructor TForm1.Destroy;
begin
  FAlertView.Free;
  FMediaManager.Free;
  inherited;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  TakenPhotoImage.Height := Screen.Height * 0.3;
  PhotosListBox.Width := Screen.Width * 0.4;
end;

procedure TForm1.AlbumReset;
begin
  CollectionImage.Bitmap.Assign(nil);
  PhotosListBox.Items.Clear;
end;

procedure TForm1.LoadAlbumsComboBox(const ASelectID: string = '');
var
  I: Integer;
begin
  AlbumReset;
  AlbumsComboBox.Items.Clear;
  for I := 0 to FMediaManager.Collections.Count - 1 do
  begin
    AlbumsComboBox.Items.Add(FMediaManager.Collections[I].Title);
    if FMediaManager.Collections[I].ID.Equals(ASelectID) then
      AlbumsComboBox.ItemIndex := I;
  end;
  if (AlbumsComboBox.ItemIndex = -1) and (AlbumsComboBox.Items.Count > 0) then
    AlbumsComboBox.ItemIndex := 0;
end;

procedure TForm1.PhotosAuthorizationStatusHandler(Sender: TObject; const AStatus: TAuthorizationStatus);
begin
  if AStatus = TAuthorizationStatus.Authorized then
  begin
    FMediaManager.Collections.Load;
    LoadAlbumsComboBox;
  end;
end;

procedure TForm1.AlbumSelected(const AReload: Boolean = False);
var
  LCollection: TMediaCollection;
  I: Integer;
begin
  AlbumReset;
  LCollection := FMediaManager.Collections[AlbumsComboBox.ItemIndex];
  if (LCollection.Count = 0) or AReload then
    LCollection.Load;
  PhotosListBox.Items.BeginUpdate;
  try
    for I := 0 to LCollection.Count - 1 do
      PhotosListBox.Items.Add(LCollection[I].FileName);
  finally
    PhotosListBox.Items.EndUpdate;
  end;
  if PhotosListBox.Items.Count > 0 then
  begin
    PhotosListBox.ItemIndex := 0;
    PhotoSelected;
  end;
end;

procedure TForm1.PhotoSelected;
var
  LCollection: TMediaCollection;
  LItem: TMediaItem;
begin
  LCollection := FMediaManager.Collections[AlbumsComboBox.ItemIndex];
  LItem := LCollection[PhotosListBox.ItemIndex];
  UpdatePhotoLabels(LItem);
  LItem.Load(
    procedure
    begin
      CollectionImage.Bitmap.LoadFromStream(LItem.Image);
    end
  );
end;

procedure TForm1.UpdatePhotoLabels(const AItem: TMediaItem);
const
  cBooleanCaptions: array[Boolean] of string = ('No', 'Yes');
begin
  PhotoCreatedDateLabel.Text := FormatDateTime('yyyy/mm/dd hh:nn:ss', AItem.DateTime);
  if (AItem.Location.Latitude <> 0) and (AItem.Location.Longitude <> 0) then
    PhotoLocationLabel.Text := Format('%.5f, %.5f', [AItem.Location.Latitude, AItem.Location.Longitude])
  else
    PhotoLocationLabel.Text := '';
  PhotoFavoriteLabel.Text := cBooleanCaptions[AItem.Favorite];
  PhotoHiddenLabel.Text := cBooleanCaptions[AItem.Hidden];
end;

procedure TForm1.PhotoLoadedHandler(Sender: TObject);
var
  LItem: TMediaItem;
begin
  LItem := TMediaItem(Sender);
  CollectionImage.Bitmap.LoadFromStream(LItem.Image);
end;

procedure TForm1.PhotosListBoxItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
  PhotoSelected;
end;

procedure TForm1.SavePhotoActionExecute(Sender: TObject);
var
  LCollection: TMediaCollection;
  LFileName: string;
begin
  LFileName := TPath.ChangeExtension(TPath.GetTempFileName, cJPEGExtension);
  LCollection := FMediaManager.Collections[AlbumsComboBox.ItemIndex];
  TakenPhotoImage.Bitmap.SaveToFile(LFileName);
  LCollection.SaveItem(LFileName,
    procedure(const AItem: TMediaItem; const AError: string)
    begin
      TFile.Delete(LFileName);
      if AError.IsEmpty then
      begin
        AlbumSelected(True);
        ShowMessage('Successfully saved photo to ' + LCollection.Title);
      end
      else
        ShowMessage('Saving photo failed: ' + AError)
    end
  );
end;

procedure TForm1.SavePhotoActionUpdate(Sender: TObject);
begin
  SavePhotoAction.Enabled := not TakenPhotoImage.Bitmap.IsEmpty and (AlbumsComboBox.ItemIndex > -1);
end;

procedure TForm1.TakePhotoButtonClick(Sender: TObject);
begin
  TakePhotoFromCameraAction.ExecuteTarget(TakePhotoButton);
end;

procedure TForm1.TakePhotoFromCameraActionDidFinishTaking(Image: TBitmap);
begin
  TakenPhotoImage.Bitmap.Assign(Image);
end;

procedure TForm1.TestChangeActionExecute(Sender: TObject);
var
  LCollection: TMediaCollection;
  LItem: TMediaItem;
begin
  LCollection := FMediaManager.Collections[AlbumsComboBox.ItemIndex];
  LItem := LCollection[PhotosListBox.ItemIndex];
  LItem.Favorite := not LItem.Favorite;
  LItem.DateTime := Now;
  LItem.UpdateProperties(
    procedure(const AError: string)
    begin
      if AError.IsEmpty then
      begin
        UpdatePhotoLabels(LItem);
        ShowMessage('Successfully updated photo');
      end
      else
        ShowMessage('Updating photo failed: ' + AError);
    end
  );
end;

procedure TForm1.TestChangeActionUpdate(Sender: TObject);
begin
  TestChangeAction.Enabled := PhotosListBox.ItemIndex > -1;
end;

procedure TForm1.AddCollectionActionExecute(Sender: TObject);
begin
  if not FMediaManager.Collections.CollectionTitleExists(CollectionNameEdit.Text) then
  begin
    FMediaManager.Collections.NewCollection(CollectionNameEdit.Text,
      procedure(const ACollection: TMediaCollection; const AError: string)
      begin
        if AError.IsEmpty then
        begin
          LoadAlbumsComboBox(ACollection.ID);
          ShowMessage('Successfully created album');
        end
        else
          ShowMessage('Creating album failed: ' + AError);
      end
    );
  end
  else
    ShowMessage('An album with the title "' + CollectionNameEdit.Text + '" already exists');
end;

procedure TForm1.AddCollectionActionUpdate(Sender: TObject);
begin
  AddCollectionAction.Enabled := not CollectionNameEdit.Text.Trim.IsEmpty;
end;

procedure TForm1.DeleteCollectionActionUpdate(Sender: TObject);
begin
  // Good question - should it be only one of "My Albums"? - apparently, yes
end;

procedure TForm1.DeletePhotoHandler(Sender: TObject);
var
  LCollection: TMediaCollection;
  LItems: TArray<TMediaItem>;
  I: Integer;
begin
  LCollection := FMediaManager.Collections[AlbumsComboBox.ItemIndex];
  for I := 0 to PhotosListBox.Items.Count - 1 do
  begin
    if PhotosListBox.ListItems[I].IsSelected then
      LItems := LItems + [LCollection[I]];
  end;
  FMediaManager.DeleteItems(LItems,
    procedure(const AError: string)
    begin
      // Do NOT attempt to reference LPhoto here, since if will have been destroyed if AError is blank
      if AError.IsEmpty then
        TThread.ForceQueue(nil, DeleteSelectedPhotoListBoxItems)
      else
        ShowMessage('Error deleting photos: ' + AError);
    end
  );
end;

procedure TForm1.DeleteSelectedPhotoListBoxItems;
begin
  // I am having problems forcing the UI to update when a deletion handler returns. No idea as to a proper solution as yet
  AlbumSelected(True);
end;

procedure TForm1.DeletePhotoActionExecute(Sender: TObject);
begin
  FAlertView.ClearActions;
  FAlertView.AddAction('Delete', DeletePhotoHandler, TAlertActionStyle.Destructive);
  FAlertView.Show('Delete Selected Photos?');
end;

procedure TForm1.DeletePhotoActionUpdate(Sender: TObject);
begin
  DeletePhotoAction.Enabled := PhotosListBox.SelectedCount > 0;
end;

procedure TForm1.AlbumsComboBoxChange(Sender: TObject);
begin
  if AlbumsComboBox.ItemIndex > -1 then
    AlbumSelected;
end;

end.
