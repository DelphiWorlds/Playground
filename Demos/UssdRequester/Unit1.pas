unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.ScrollBox,
  FMX.Memo, FMX.Memo.Types,
  DW.UssdRequester;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    RequestEdit: TEdit;
    Label2: TLabel;
    SendButton: TButton;
    ResponsesMemo: TMemo;
    procedure SendButtonClick(Sender: TObject);
  private
    FUssdRequester: TUssdRequester;
    procedure RequesterUssdResponseHandler(Sender: TObject; const ARequest, AResponse: string; const AFailure: TUssdFailure);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.Permissions,
  DW.Permissions.Helpers;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FUssdRequester := TUssdRequester.Create;
  FUssdRequester.OnUssdResponse := RequesterUssdResponseHandler;
  PermissionsService.RequestPermissions(['android.permission.CALL_PHONE'],
    procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
    begin
      SendButton.Enabled := AGrantResults.AreAllGranted;
    end
  );
end;

procedure TForm1.RequesterUssdResponseHandler(Sender: TObject; const ARequest, AResponse: string; const AFailure: TUssdFailure);
begin
  case AFailure of
    TUssdFailure.None:
      ResponsesMemo.Lines.Add('Success! Response: ' + AResponse);
    TUssdFailure.ReturnFailure:
      ResponsesMemo.Lines.Add('Network failed to fulfill request: ' + ARequest);
    TUssdFailure.ServiceUnavailable:
      ResponsesMemo.Lines.Add('Ussd service unavailable when sending request: ' + ARequest);
    TUssdFailure.Unknown:
      ResponsesMemo.Lines.Add('Ussd service failed for unknown reason when sending request: ' + ARequest);
  end;
end;

procedure TForm1.SendButtonClick(Sender: TObject);
begin
  FUssdRequester.SendUssdRequest(RequestEdit.Text);
end;

end.
