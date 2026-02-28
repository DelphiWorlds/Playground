unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

uses
  DW.GrijjyErrorReporting;

{$R *.fmx}

procedure CauseAV;
var
  P: PInteger;
  I: Integer;
begin
  { Create an Access Violation by accessing an invalid address. }
  P := Pointer(-1);
  I := P^;
  P^ := I + 1;
end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  ExceptionInterceptor.AddHandler(
    procedure(const Report: IgoExceptionReport)
    begin
      TThread.Queue(nil, procedure begin ShowMessage(Report.Report); end);
    end
  );
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // **NOTE**: Since the DW.GrijjyErrorReporting unit has been added to the project, the user will NOT see the exception, unless it is handled (see the Create method for an example)
  CauseAV;
end;

end.
