unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox,
  DW.ExoPlayer;

type
  TForm1 = class(TForm)
    PlayerLayout: TLayout;
    ButtonsLayout: TLayout;
    PlayButton: TButton;
    VideosListBox: TListBox;
    StopButton: TButton;
    HideControlsCheckBox: TCheckBox;
    procedure PlayButtonClick(Sender: TObject);
    procedure HideControlsCheckBoxChange(Sender: TObject);
    procedure VideosListBoxChange(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
  private
    FExoPlayer: TExoPlayer;
    FVideos: TArray<string>;
    procedure AddVideos;
    procedure PrepareVideo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FExoPlayer := TExoPlayer.Create;
  FExoPlayer.View.Parent := PlayerLayout;
  FExoPlayer.ControllerTimeout := 1000; // ms. Set to 0 to prevent player controls from showing
  AddVideos;
end;

procedure TForm1.AddVideos;
var
  LVideo: string;
begin
  FVideos := [
    'https://storage.googleapis.com/exoplayer-test-media-0/BigBuckBunny_320x180.mp4',
    'https://commondatastorage.googleapis.com/gtv-videos-bucket/sample/ForBiggerBlazes.mp4',
    'https://commondatastorage.googleapis.com/gtv-videos-bucket/sample/ForBiggerEscapes.mp4',
    'https://commondatastorage.googleapis.com/gtv-videos-bucket/sample/ForBiggerFun.mp4',
    'https://commondatastorage.googleapis.com/gtv-videos-bucket/sample/ForBiggerJoyrides.mp4'
  ];
  for LVideo in FVideos do
    VideosListBox.Items.AddStrings(LVideo.Substring(LVideo.LastIndexOf('/') + 1));
  VideosListBox.ItemIndex := 0;
  PrepareVideo;
end;

destructor TForm1.Destroy;
begin
  FExoPlayer.Free;
  inherited;
end;

procedure TForm1.HideControlsCheckBoxChange(Sender: TObject);
begin
  FExoPlayer.UseController := not HideControlsCheckBox.IsChecked;
end;

procedure TForm1.PlayButtonClick(Sender: TObject);
begin
  FExoPlayer.Play;
end;

procedure TForm1.PrepareVideo;
begin
  FExoPlayer.Prepare(FVideos[VideosListBox.ItemIndex]);
end;

procedure TForm1.StopButtonClick(Sender: TObject);
begin
  FExoPlayer.Stop;
end;

procedure TForm1.VideosListBoxChange(Sender: TObject);
begin
  FExoPlayer.Stop;
  PrepareVideo;
end;

end.
