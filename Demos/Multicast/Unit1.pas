unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo.Types,
  DW.Network.Types, DW.Network.Multicast, DW.Network.Provider;

type
  TForm1 = class(TForm, INetworkListener)
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
    procedure SendCheckBoxChange(Sender: TObject);
    procedure ReceiveCheckBoxChange(Sender: TObject);
  private
    FReceiver: IMulticastReceiver;
    FSender: IMulticastSender;
    procedure DumpLocalAddresses;
    procedure ReceiverDataReceivedHandler(Sender: TObject; const AIP: string; const AData: TBytes);
  public
    { INetworkListener }
    procedure LocalAddressesChange;
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
  FSender := MulticastProvider.CreateSender;
  FReceiver := MulticastProvider.CreateReceiver;
  FReceiver.Port := 64220;
  FReceiver.OnDataReceived := ReceiverDataReceivedHandler;
  NetworkProvider.AddListener(Self);
  DumpLocalAddresses;
end;

destructor TForm1.Destroy;
begin
  NetworkProvider.RemoveListener(Self);
  inherited;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TForm1.DumpLocalAddresses;
var
  LLocalAddress: TIPAddress;
begin
  LocalIPMemo.Lines.Clear;
  for LLocalAddress in NetworkProvider.GetLocalAddresses do
    LocalIPMemo.Lines.Add(Format('%s on %s (index: %d)', [LLocalAddress.IP, LLocalAddress.InterfaceName, LLocalAddress.InterfaceIndex]));
end;

procedure TForm1.LocalAddressesChange;
begin
  DumpLocalAddresses;
end;

procedure TForm1.ReceiveCheckBoxChange(Sender: TObject);
begin
  FReceiver.IsActive := ReceiveCheckBox.IsChecked;
end;

procedure TForm1.ReceiverDataReceivedHandler(Sender: TObject; const AIP: string; const AData: TBytes);
begin
  TThread.ForceQueue(nil,
    procedure
    begin
      Memo1.Lines.Add(FormatDateTime('yyyy/mm/dd hh:nn:ss', Now) + ' IP: ' + AIP + ', Data: ' + TEncoding.Default.GetString(AData));
    end
  );
end;

procedure TForm1.SendCheckBoxChange(Sender: TObject);
begin
  Timer.Enabled := SendCheckBox.IsChecked;
end;

procedure TForm1.TimerTimer(Sender: TObject);
begin
  FSender.Broadcast('TEST', 64220);
end;

end.
