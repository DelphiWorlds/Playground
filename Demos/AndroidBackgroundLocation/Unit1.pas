unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.JSON,
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo,
  DW.BroadcastMessage.Receiver,
  DW.LocationService.Manager, DW.LocationService.Common;

type
  TForm1 = class(TForm)
    ButtonsLayout: TLayout;
    StartStopLocationButton: TButton;
    MessagesMemo: TMemo;
    procedure StartStopLocationButtonClick(Sender: TObject);
  private
    FMessageReceiver: TJSONBroadcastMessageReceiver;
    FPreferences: ILocationPreferences;
    FServiceManager: ILocationServiceManager;
    procedure ServiceMessageHandler(const AKind: Integer; const AMsg: TJSONValue);
    procedure UpdateStartStopLocationButton;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.App,
  DW.OSLog, DW.JSON, DW.BroadcastMessage.Sender,
  DW.LocationPermissions, DW.Location.Types;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FServiceManager := TLocationServiceManager.Create('ABLDemoService');
  FPreferences := TLocationPreferences.Create;
  FMessageReceiver := TJSONBroadcastMessageReceiver.Create(ServiceMessageHandler);
  UpdateStartStopLocationButton;
end;

destructor TForm1.Destroy;
begin
  //
  inherited;
end;

procedure TForm1.StartStopLocationButtonClick(Sender: TObject);
begin
  if not FPreferences.GetIsActive then
  begin
    LocationPermissions.RequestBackground(
      procedure(const ACanStart: Boolean)
      begin
        if ACanStart then
          FServiceManager.StartLocationUpdates
      end
    );
  end
  else
    FServiceManager.StopLocationUpdates;
end;

procedure TForm1.UpdateStartStopLocationButton;
const
  cButtonCaptions: array[Boolean] of string = ('Start Location', 'Stop Location');
begin
  StartStopLocationButton.Text := cButtonCaptions[FPreferences.GetIsActive];
end;

procedure TForm1.ServiceMessageHandler(const AKind: Integer; const AMsg: TJSONValue);
const
  cActiveCaptions: array[Boolean] of string = ('Inactive', 'Active');
var
  LLocationData: TLocationData;
begin
  case AKind of
    0:
    begin
      TOSLog.d('> Location received in app');
      MessagesMemo.Lines.Add(TJSONHelper.Tidy(AMsg));
      LLocationData.FromJSONValue(AMsg);
      // Location properties can now be accessed from LLocationData
    end;
    1:
    begin
      TOSLog.d('> Location updates changed in app');
      MessagesMemo.Lines.Add('Location updates changed to: ' + cActiveCaptions[FPreferences.GetIsActive]);
      UpdateStartStopLocationButton;
    end;
  end;
end;

end.
