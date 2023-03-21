unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  DW.RunProcess.Win;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    FProcess: TRunProcess;
    procedure ProcessOutputHandler(Sender: TObject; const AOutput: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FProcess.CommandLine := 'cmd /c dir C:\Temp';
//  FProcess.Run;
  if FProcess.RunAndWait(100) <> 0 then
    ShowMessage('Timeout or failed to start')
  else
    ShowMessage('Success!');
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FProcess := TRunProcess.Create;
  FProcess.ShowCommandInLog := True;
  FProcess.OnProcessOutput := ProcessOutputHandler;
end;

destructor TForm1.Destroy;
begin
  FProcess.Free;
  inherited;
end;

procedure TForm1.ProcessOutputHandler(Sender: TObject; const AOutput: string);
begin
  Memo1.Lines.Add(AOutput);
end;

end.
