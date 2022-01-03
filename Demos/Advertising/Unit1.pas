unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo,
  DW.Advertising;

type
  TForm1 = class(TForm)
    Memo: TMemo;
    ButtonsLayout: TLayout;
    ShowButton: TButton;
    procedure ShowButtonClick(Sender: TObject);
  private
    FAd: TInterstitialAd;
    procedure AdDismissedFullScreenContentHandler(Sender: TObject);
    procedure AdFailedToShowFullScreenContentHandler(Sender: TObject; const AError: TAdError);
    procedure AdShowedFullScreenContentHandler(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FAd := TInterstitialAd.Create;
  FAd.AdUnitId := 'ca-app-pub-3940256099942544/1033173712'; // Test
  FAd.OnAdDismissedFullScreenContent := AdDismissedFullScreenContentHandler;
  FAd.OnAdFailedToShowFullScreenContent := AdFailedToShowFullScreenContentHandler;
  FAd.OnAdShowedFullScreenContent := AdShowedFullScreenContentHandler;
end;

procedure TForm1.ShowButtonClick(Sender: TObject);
begin
  FAd.Load;
end;

procedure TForm1.AdDismissedFullScreenContentHandler(Sender: TObject);
begin
  Memo.Lines.Add('Ad Dismissed');
end;

procedure TForm1.AdFailedToShowFullScreenContentHandler(Sender: TObject; const AError: TAdError);
begin
  Memo.Lines.Add('Ad Failed - ' + AError.ErrorCode.ToString + ': ' + AError.Message);
end;

procedure TForm1.AdShowedFullScreenContentHandler(Sender: TObject);
begin
  Memo.Lines.Add('Ad Shown');
end;

end.
