unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Edit, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo;

type
  TStoredParams = record
    P8FileName: string;
    KeyID: string;
    TeamID: string;
    BundleID: string;
    Token: string;
    constructor Create(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
  end;

  TForm1 = class(TForm)
    SendButton: TButton;
    ButtonsLayout: TLayout;
    P8FileLayout: TLayout;
    P8FileLabel: TLabel;
    P8FileNameEdit: TEdit;
    P8FileButton: TEllipsesEditButton;
    MessageTextLayout: TLayout;
    BodyLabel: TLabel;
    BodyMemo: TMemo;
    SubtitleEdit: TEdit;
    SubtitleLabel: TLabel;
    TitleEdit: TEdit;
    TitleLabel: TLabel;
    ImageURLLabel: TLabel;
    ImageURLEdit: TEdit;
    TokenEdit: TEdit;
    ClearTokenEditButton: TClearEditButton;
    TokenLabel: TLabel;
    TeamIDLabel: TLabel;
    TeamIDEdit: TEdit;
    KeyLabel: TLabel;
    KeyIDEdit: TEdit;
    P8OpenDialog: TOpenDialog;
    BundleIDLabel: TLabel;
    BundleIDEdit: TEdit;
    procedure SendButtonClick(Sender: TObject);
    procedure P8FileButtonClick(Sender: TObject);
    procedure ClearTokenEditButtonClick(Sender: TObject);
    procedure ParamsChange(Sender: TObject);
  private
    FIsCreating: Boolean;
    FStoredParams: TStoredParams;
    FStoredParamsFileName: string;
    procedure WriteStoredParams;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils, System.JSON,
  FMX.DialogService.Async,
  DW.APNS, DW.APNSSender,
  DW.JWT.Types, DW.JWT.Grijjy;

{ TStoredParams }

constructor TStoredParams.Create(const AFileName: string);
var
  LJSON: TJSONValue;
begin
  if TFile.Exists(AFileName) then
  begin
    LJSON := TJSONObject.ParseJSONValue(TFile.ReadAllText(AFileName));
    if LJSON <> nil then
    try
      LJSON.TryGetValue('P8FileName', P8FileName);
      LJSON.TryGetValue('KeyID', KeyID);
      LJSON.TryGetValue('TeamID', TeamID);
      LJSON.TryGetValue('BundleID', BundleID);
      LJSON.TryGetValue('Token', Token);
    finally
      LJSON.Free;
    end;
  end;
end;

procedure TStoredParams.SaveToFile(const AFileName: string);
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('P8FileName', P8FileName);
    LJSON.AddPair('KeyID', KeyID);
    LJSON.AddPair('TeamID', TeamID);
    LJSON.AddPair('BundleID', BundleID);
    LJSON.AddPair('Token', Token);
    TFile.WriteAllText(AFileName, LJSON.ToJSON);
  finally
    LJSON.Free;
  end;
end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FIsCreating := True;
  try
    FStoredParamsFileName := TPath.Combine(TPath.Combine(TPath.GetDocumentsPath, 'ApplePush'), 'StoredParams.json');
    FStoredParams := TStoredParams.Create(FStoredParamsFileName);
    P8FileNameEdit.Text := FStoredParams.P8FileName;
    KeyIDEdit.Text := FStoredParams.KeyID;
    TeamIDEdit.Text := FStoredParams.TeamID;
    BundleIDEdit.Text := FStoredParams.BundleID;
    TokenEdit.Text := FStoredParams.Token;
  finally
    FIsCreating := False;
  end;
end;

procedure TForm1.WriteStoredParams;
begin
  ForceDirectories(TPath.GetDirectoryName(FStoredParamsFileName));
  FStoredParams.P8FileName := P8FileNameEdit.Text;
  FStoredParams.KeyID := KeyIDEdit.Text;
  FStoredParams.TeamID := TeamIDEdit.Text;
  FStoredParams.BundleID := BundleIDEdit.Text;
  FStoredParams.Token := TokenEdit.Text;
  FStoredParams.SaveToFile(FStoredParamsFileName);
end;

procedure TForm1.ClearTokenEditButtonClick(Sender: TObject);
begin
  TokenEdit.Text := '';
  WriteStoredParams;
end;

procedure TForm1.P8FileButtonClick(Sender: TObject);
var
  LFileName: string;
begin
  if P8OpenDialog.Execute then
  begin
    P8FileNameEdit.Text := P8OpenDialog.FileName;
    if KeyIDEdit.Text.IsEmpty then
    begin
      LFileName := TPath.GetFileNameWithoutExtension(TPath.GetFileName(P8FileNameEdit.Text));
      if LFileName.StartsWith('AuthKey_') then
        KeyIDEdit.Text := LFileName.Substring(8);
    end;
    WriteStoredParams;
  end;
end;

procedure TForm1.SendButtonClick(Sender: TObject);
var
  LCreator: IJWTCreator;
  LParams: TJWTCreatorParams;
  LMessage: IAPNSMessage;
  LMessageParams: TAPNSMessageParams;
begin
  LMessage := TAPNSMessage.Create;
  LMessage.Category := 'IMAGE_AND_TEXT';
  LMessage.Title := TitleEdit.Text;
  LMessage.Body := BodyMemo.Text;
  LMessage.ImageUrl := ImageURLEdit.Text;
  LParams.Algorithm := 'ES256';
  LParams.IssuerID := TeamIDEdit.Text;
  LParams.KeyID := KeyIDEdit.Text;
  LParams.Secret := TFile.ReadAllText(P8FileNameEdit.Text);
  LCreator := TJWTCreator.Create(LParams);
  LMessageParams := TAPNSMessageParams.Create(LCreator.GetJWT(60), BundleIDEdit.Text, TokenEdit.Text);
  LMessageParams.IsProduction := False; // Set to True when using production
  if APNSSender.PostMessage(LMessageParams, LMessage.ToJSON) then
    TDialogServiceAsync.MessageDialog('Sent push', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0)
  else
    TDialogServiceAsync.MessageDialog('Send failed', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);
end;

procedure TForm1.ParamsChange(Sender: TObject);
begin
  if not FIsCreating then
    WriteStoredParams;
end;

end.
