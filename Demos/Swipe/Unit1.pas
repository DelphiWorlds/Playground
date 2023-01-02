unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls,
  DW.SwipeView;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    SwipeView1: TSwipeView;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  Image1.Parent := SwipeView1.Layout[TLayoutPosition.Left];
  Image1.Align := TAlignLayout.Client;
  Image2.Parent := SwipeView1.Layout[TLayoutPosition.Center];
  Image2.Align := TAlignLayout.Client;
  Image3.Parent := SwipeView1.Layout[TLayoutPosition.Right];
  Image3.Align := TAlignLayout.Client;
  // Works around an apparent bug in Delphi
  SwipeView1.Width := Width;
end;

end.
