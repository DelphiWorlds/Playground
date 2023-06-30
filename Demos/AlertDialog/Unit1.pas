unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  DW.AlertDialog;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FDialog: IAlertDialog;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Label1.Text := '';
  if FDialog = nil then
    FDialog := AlertDialogProvider.GetDialog;
  FDialog.ClearActions;
  FDialog.AddAction('One');
  FDialog.AddAction('Two');
  FDialog.AddAction('Three');
  FDialog.AddAction('Four');
  FDialog.Show('Selection', 'Choose one',
    procedure(const AActionIndex: Integer)
    begin
      if AActionIndex = -2 then
        Label1.Text := 'Cancel was tapped'
      else
        Label1.Text := Format('Item %d was tapped', [AActionIndex + 1]);
    end
  );
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Label1.Text := '';
  if FDialog = nil then
    FDialog := AlertDialogProvider.GetDialog;
  FDialog.ClearActions;
  FDialog.Show('Warning', 'Click OK to confirm, Cancel to um... cancel?',
    procedure(const AActionIndex: Integer)
    begin
      if AActionIndex = -2 then
        Label1.Text := 'Cancel was tapped'
      else if AActionIndex = -1 then
        Label1.Text := 'OK was tapped';
    end
  );
end;

end.
