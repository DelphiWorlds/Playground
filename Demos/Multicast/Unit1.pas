unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo.Types,
  DW.Multicaster, DW.Network;

type
  TForm1 = class(TForm, INetworkNotifier)
    Timer: TTimer;
    Memo1: TMemo;
    Button1: TButton;
    Layout1: TLayout;
    Label1: TLabel;
    Layout2: TLayout;
    Label2: TLabel;
    LocalIPMemo: TMemo;
    SendCheckBox: TCheckBox;
    ReceiveCheckBox: TCheckBox;
    procedure TimerTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SendSwitchSwitch(Sender: TObject);
    procedure ReceiveSwitchSwitch(Sender: TObject);
  private
    FMulticaster: TMulticaster;
    procedure DumpLocalAddresses;
    procedure MulticasterDataReceivedHandler(Sender: TObject; const AIP: string; const AData: TBytes);
  public
    { INetworkNotifier }
    procedure LocalAddressesChange(const ALocalAddresses: TLocalAddresses);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  DW.OSLog;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FMulticaster := TMulticaster.Create;
  FMulticaster.Port := 64220;
  FMulticaster.OnDataReceived := MulticasterDataReceivedHandler;
  TNetwork.AddNotifier(Self);
  DumpLocalAddresses;
end;

destructor TForm1.Destroy;
begin
  TNetwork.RemoveNotifier(Self);
  inherited;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TForm1.DumpLocalAddresses;
var
  LLocalAddress: TLocalAddress;
begin
  LocalIPMemo.Lines.Clear;
  for LLocalAddress in TNetwork.GetLocalAddresses do
    LocalIPMemo.Lines.Add(Format('%s on %s (%d)', [LLocalAddress.IP, LLocalAddress.InterfaceName, LLocalAddress.InterfaceIndex]));
end;

procedure TForm1.LocalAddressesChange(const ALocalAddresses: TLocalAddresses);
begin
  DumpLocalAddresses;
end;

procedure TForm1.MulticasterDataReceivedHandler(Sender: TObject; const AIP: string; const AData: TBytes);
begin
  TThread.ForceQueue(nil,
    procedure
    begin
      Memo1.Lines.Add(FormatDateTime('yyyy/mm/dd hh:nn:ss', Now) + ' IP: ' + AIP + ', Data: ' + TEncoding.Default.GetString(AData));
    end
  );
end;

procedure TForm1.ReceiveSwitchSwitch(Sender: TObject);
begin
  FMulticaster.Active := ReceiveCheckBox.IsChecked;
end;

procedure TForm1.SendSwitchSwitch(Sender: TObject);
begin
  Timer.Enabled := SendCheckBox.IsChecked;
end;

procedure TForm1.TimerTimer(Sender: TObject);
begin
  FMulticaster.Broadcast('TEST', FMulticaster.Port);
end;

end.
