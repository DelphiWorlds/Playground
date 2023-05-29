unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Actions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ActnList,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Media,
  // DW.ImageProcessor,
  DW.ShareItems,
  DW.BarcodeReader.Types,
  DW.VideoPlayer,
  DW.SimpleCamera;

type
  TForm1 = class(TForm, IBarcodeReader)
    RecordButton: TButton;
    PreviewLayout: TLayout;
    Label1: TLabel;
    ButtonsLayout: TLayout;
    PlaybackButton: TButton;
    ActionList: TActionList;
    RecordAction: TAction;
    PlaybackAction: TAction;
    MediaPlayerControl1: TMediaPlayerControl;
    MediaPlayer1: TMediaPlayer;
    procedure RecordActionExecute(Sender: TObject);
    procedure RecordActionUpdate(Sender: TObject);
    procedure PlaybackActionUpdate(Sender: TObject);
    procedure PlaybackActionExecute(Sender: TObject);
  private
    FCamera: TSimpleCamera;
    FPlayer: TVideoPlayer;
    FCaptureFileName: string;
    FShare: TShareItems;
  public
    function GetFormats: TBarcodeFormats;
    procedure ReceivedBarcodes(const ABarcodes: TBarcodes; const AError: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  // DW.BarcodeImageProcessor.iOS,
  // DW.ZXingImageProcessor.iOS,
  DW.OSLog;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FCaptureFileName := TPath.Combine(TPath.GetDocumentsPath, 'test.mp4');
  FCamera := TSimpleCamera.Create;
  FCamera.Preview.Parent := PreviewLayout;
  // FCamera.ImageProcessor := TPlatformBarcodeImageProcessor.Create(Self);
  // FCamera.ImageProcessor := TPlatformZXingImageProcessor.Create(Self);
  FPlayer := TVideoPlayer.Create;
  FPlayer.PlayerView.Parent := PreviewLayout;
end;

destructor TForm1.Destroy;
begin
  FCamera.Free;
  inherited;
end;

function TForm1.GetFormats: TBarcodeFormats;
begin
  Result := [TBarcodeFormat.All];
end;

procedure TForm1.RecordActionExecute(Sender: TObject);
begin
  if FCamera.IsRecording then
    FCamera.StopRecording
  else
  begin
    FPlayer.IsVisible := False;
    FCamera.IsVisible := True;
    FCamera.StartRecording(FCaptureFileName);
  end;
end;

procedure TForm1.RecordActionUpdate(Sender: TObject);
const
  cRecordCaptions: array[Boolean] of string = ('Record', 'Stop Recording');
begin
  RecordAction.Enabled := not FPlayer.IsPlaying;
  RecordAction.Text := cRecordCaptions[FCamera.IsRecording];
end;

procedure TForm1.PlaybackActionExecute(Sender: TObject);
begin
  if FPlayer.IsPlaying then
    FPlayer.Stop
  else
  begin
    FCamera.IsVisible := False;
    FPlayer.IsVisible := True;
    FPlayer.Play(FCaptureFileName);
//    FPlayer.IsVisible := False;
//    MediaPlayer1.FileName := FCaptureFileName;
//    MediaPlayer1.Play;
//    if FShare = nil then
//      FShare := TShareItems.Create;
//    FShare.AddFile(FCaptureFileName);
//    FShare.Share(PlaybackButton);
  end;
end;

procedure TForm1.PlaybackActionUpdate(Sender: TObject);
const
  cPlaybackCaptions: array[Boolean] of string = ('Playback', 'Stop Playback');
begin
  PlaybackAction.Enabled := not FCamera.IsRecording and TFile.Exists(FCaptureFileName);
  PlaybackAction.Text := cPlaybackCaptions[FPlayer.IsPlaying];
end;

procedure TForm1.ReceivedBarcodes(const ABarcodes: TBarcodes; const AError: string);
begin
  if not AError.IsEmpty then
    TOSLog.d('Error: %s', [AError])
  else if Length(ABarcodes) > 0 then
  begin
    TOSLog.d('Barcode: %s', [ABarcodes[0].Value]);
    Label1.Text := ABarcodes[0].Value;
    FCamera.StopRecording;
  end;
//  else
//    TOSLog.d('No barcodes or error!');
end;

end.
