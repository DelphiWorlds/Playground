unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, FMX.Objects,
  FMX.Media,
  DW.VKObserver;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    ContentLayout: TLayout;
    ControlsLayout: TLayout;
    Image1: TImage;
    Edit2: TEdit;
    procedure ContentLayoutResize(Sender: TObject);
    procedure ContentLayoutResized(Sender: TObject);
  private
    FVKObserver: TVKObserver;
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
  FVKObserver := TVKObserver.Create;
  FVKObserver.AddContainer(ControlsLayout);
end;

destructor TForm1.Destroy;
begin
  FVKObserver.Free;
  inherited;
end;

procedure TForm1.ContentLayoutResize(Sender: TObject);
begin
  TOSLog.d('TForm1.ContentLayoutResize');
end;

procedure TForm1.ContentLayoutResized(Sender: TObject);
begin
  TOSLog.d('TForm1.ContentLayoutResized');
end;

end.
