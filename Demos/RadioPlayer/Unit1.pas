unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  DW.RadioPlayer;

type
  TForm1 = class(TForm)
    ButtonsLayout: TLayout;
    StopButton: TButton;
    PlayButton: TButton;
    MetadataLabel: TLabel;
    procedure PlayButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
  private
    FRadio: TRadioPlayer;
    procedure RadioStatusChangedHandler(Sender: TObject);
    procedure RadioServiceStartedHandler(Sender: TObject);
    procedure RadioStreamMetadataHandler(Sender: TObject; const AMetadata: string);
    procedure UpdateButtons;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

// A couple of streaming URLs:

// Majestic jukebox: http://uk3.internet-radio.com:8405/live
// The Groove: http://uk7.internet-radio.com:8352/stream
// ABC Adelaide: http://live-radio01.mediahubaustralia.com/5LRW/aac
// Plonsk: https://sluchaj.link:5443/plonsk

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FRadio := TRadioPlayer.Create;
  FRadio.URL := 'http://uk3.internet-radio.com:8405/live';
  // Set UseService to False if you don't want to use a service (i.e. applies to Android only)
  // FRadio.UseService := False;
  FRadio.OnServiceStarted := RadioServiceStartedHandler;
  FRadio.OnStatusChanged := RadioStatusChangedHandler;
  FRadio.OnStreamMetadata := RadioStreamMetadataHandler;
  // Only applies to Android only, but calling it to cater for when it *is* Android
  FRadio.StartService('RadioService');
end;

destructor TForm1.Destroy;
begin
  FRadio.Free;
  inherited;
end;

procedure TForm1.RadioServiceStartedHandler(Sender: TObject);
begin
  PlayButton.Enabled := True;
end;

procedure TForm1.RadioStatusChangedHandler(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TForm1.RadioStreamMetadataHandler(Sender: TObject; const AMetadata: string);
begin
  MetadataLabel.Text := FRadio.ParseStreamTitle(AMetadata, 'Unknown title');
end;

procedure TForm1.PlayButtonClick(Sender: TObject);
begin
  case FRadio.Status of
    TRadioStatus.Paused, TRadioStatus.Stopped:
      FRadio.Play;
    TRadioStatus.Playing:
      FRadio.Pause;
  end;
end;

procedure TForm1.StopButtonClick(Sender: TObject);
begin
  FRadio.Stop;
end;

procedure TForm1.UpdateButtons;
begin
  case FRadio.Status of
    TRadioStatus.Unknown, TRadioStatus.Stopped:
    begin
      PlayButton.Enabled := True;
      PlayButton.Text := 'Play';
      StopButton.Enabled := False;
    end;
    TRadioStatus.Playing:
    begin
      PlayButton.Text := 'Pause';
      StopButton.Enabled := True;
    end;
    TRadioStatus.Paused:
      PlayButton.Text := 'Play';
  end;
end;

end.
