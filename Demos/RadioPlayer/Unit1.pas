unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  DW.RadioPlayer;

type
  TForm1 = class(TForm)
    ButtonsLayout: TLayout;
    StopButton: TButton;
    PlayButton: TButton;
    MetadataLabel: TLabel;
    StationsListBox: TListBox;
    BitRateLabel: TLabel;
    NameLabel: TLabel;
    procedure PlayButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure StationsListBoxItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
  private
    FRadio: TRadioPlayer;
    procedure LoadStations;
    procedure RadioStatusChangedHandler(Sender: TObject);
    procedure RadioServiceStartedHandler(Sender: TObject);
    procedure RadioStreamMetadataHandler(Sender: TObject; const AMetadata: TArray<string>);
    procedure UpdateControls;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils, System.JSON;

type
  TStation = record
    Name: string;
    URL: string;
    function DisplayName: string;
  end;

  TStations = TArray<TStation>;

  TStationsFile = record
  public
    Stations: TStations;
    procedure AddStation(const AElement: TJSONValue);
    procedure Load;
  end;

{ TStation }

function TStation.DisplayName: string;
begin
  Result := Format('%s (%s)', [Name, URL]);
end;

{ TStationsFile }

procedure TStationsFile.AddStation(const AElement: TJSONValue);
var
  LStation: TStation;
begin
  AElement.TryGetValue('Name', LStation.Name);
  AElement.TryGetValue('URL', LStation.URL);
  Stations := Stations + [LStation];
end;

procedure TStationsFile.Load;
var
  LFileName: string;
  LJSON, LElement: TJSONValue;
begin
  {$IF Defined(MSWINDOWS)}
  LFileName := TPath.Combine(TPath.GetDocumentsPath, 'RadioTest\stations.json');
  {$ELSE}
  LFileName := TPath.Combine(TPath.GetDocumentsPath, 'stations.json');
  {$ENDIF}
  if TFile.Exists(LFileName) then
  begin
    LJSON := TJSONObject.ParseJSONValue(TFile.ReadAllText(LFileName));
    if LJSON <> nil then
    try
      if LJSON is TJSONArray then
      begin
        for LElement in TJSONArray(LJSON) do
          AddStation(LElement);
      end;
    finally
      LJSON.Free;
    end;
  end;
end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FRadio := TRadioPlayer.Create;
  // Set UseService to False if you don't want to use a service (i.e. applies to Android only)
  // FRadio.UseService := False;
  FRadio.OnServiceStarted := RadioServiceStartedHandler;
  FRadio.OnStatusChanged := RadioStatusChangedHandler;
  FRadio.OnStreamMetadata := RadioStreamMetadataHandler;
  // Only applies to Android only, but calling it to cater for when it *is* Android
  FRadio.StartService('RadioService');
  LoadStations;
end;

destructor TForm1.Destroy;
begin
  FRadio.Free;
  inherited;
end;

procedure TForm1.LoadStations;
var
  LStationsFile: TStationsFile;
  LStation: TStation;
begin
  LStationsFile.Load;
  for LStation in  LStationsFile.Stations do
  begin
    StationsListBox.Items.Add(LStation.DisplayName);
    StationsListBox.ListItems[StationsListBox.Items.Count - 1].TagString := LStation.URL;
  end;
  if StationsListBox.Items.Count > 0 then
  begin
    StationsListBox.ItemIndex := 0;
    StationsListBoxItemClick(StationsListBox, StationsListBox.ListItems[StationsListBox.ItemIndex]);
  end;
end;

procedure TForm1.StationsListBoxItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
  FRadio.URL := Item.TagString;
end;

procedure TForm1.RadioServiceStartedHandler(Sender: TObject);
begin
  PlayButton.Enabled := True;
end;

procedure TForm1.RadioStatusChangedHandler(Sender: TObject);
begin
  UpdateControls;
end;

procedure TForm1.RadioStreamMetadataHandler(Sender: TObject; const AMetadata: TArray<string>);
var
  LValue: string;
begin
  if TRadioMetadata.GetStreamTitle(AMetadata, LValue) then
    MetadataLabel.Text := LValue;
  if TRadioMetadata.GetValue(AMetadata, cIcyName, LValue) then
    NameLabel.Text := LValue;
  if TRadioMetadata.GetValue(AMetadata, cIcyBitrate, LValue) then
    BitRateLabel.Text := Format('BitRate: %s', [LValue]);
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

procedure TForm1.UpdateControls;
begin
  case FRadio.Status of
    TRadioStatus.Unknown, TRadioStatus.Stopped:
    begin
      MetadataLabel.Text := '';
      NameLabel.Text := '';
      BitRateLabel.Text := '';
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
