unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo,
  DW.AdMob, DW.AdMobBannerAd, DW.AdMobAds;

type
  TForm1 = class(TForm)
    Memo: TMemo;
    ButtonsLayout: TLayout;
    ShowButton: TButton;
    AdMobBannerAd1: TAdMobBannerAd;
    procedure ShowButtonClick(Sender: TObject);
    procedure AdMobBannerAd1AdClicked(Sender: TObject);
    procedure AdMobBannerAd1AdClosed(Sender: TObject);
    procedure AdMobBannerAd1AdFailedToLoad(Sender: TObject; const Error: TAdError);
    procedure AdMobBannerAd1AdImpression(Sender: TObject);
    procedure AdMobBannerAd1AdLoaded(Sender: TObject);
    procedure AdMobBannerAd1AdOpened(Sender: TObject);
  private
    // FAd: TInterstitialAd;
    FAd: TAppOpenAd;
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
  // FAd := TInterstitialAd.Create;
  FAd := TAppOpenAd.Create;
  FAd.TestMode := True;
  FAd.OnAdDismissedFullScreenContent := AdDismissedFullScreenContentHandler;
  FAd.OnAdFailedToShowFullScreenContent := AdFailedToShowFullScreenContentHandler;
  FAd.OnAdShowedFullScreenContent := AdShowedFullScreenContentHandler;
end;

procedure TForm1.ShowButtonClick(Sender: TObject);
begin
  // FAd.Load;
  // AdMobBannerAd1.LoadAd;
end;

procedure TForm1.AdDismissedFullScreenContentHandler(Sender: TObject);
begin
  Memo.Lines.Add('Fullscreen Ad Dismissed');
end;

procedure TForm1.AdFailedToShowFullScreenContentHandler(Sender: TObject; const AError: TAdError);
begin
  Memo.Lines.Add('Fullscreen Ad Failed - ' + AError.ErrorCode.ToString + ': ' + AError.Message);
end;

procedure TForm1.AdShowedFullScreenContentHandler(Sender: TObject);
begin
  Memo.Lines.Add('Fullscreen Ad Shown');
end;

procedure TForm1.AdMobBannerAd1AdClicked(Sender: TObject);
begin
  Memo.Lines.Add('Ad Clicked');
end;

procedure TForm1.AdMobBannerAd1AdClosed(Sender: TObject);
begin
  Memo.Lines.Add('Ad Closed');
end;

procedure TForm1.AdMobBannerAd1AdFailedToLoad(Sender: TObject; const Error: TAdError);
begin
  Memo.Lines.Add('Ad Failed To Load');
end;

procedure TForm1.AdMobBannerAd1AdImpression(Sender: TObject);
begin
  Memo.Lines.Add('Ad Impression');
end;

procedure TForm1.AdMobBannerAd1AdLoaded(Sender: TObject);
begin
  Memo.Lines.Add('Ad Loaded');
end;

procedure TForm1.AdMobBannerAd1AdOpened(Sender: TObject);
begin
  Memo.Lines.Add('Ad Opened');
end;

end.
