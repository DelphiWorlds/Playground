unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.EditBox, FMX.SpinBox,
  LR.Types,
  DW.ElasticLayout, DW.LocationRequester;

type
  TForm1 = class(TForm)
    BottomLayout: TLayout;
    LocationMultipleButton: TButton;
    ListBox: TListBox;
    VersionLabel: TLabel;
    MultipleLayout: TLayout;
    Memo: TMemo;
    ClearButton: TButton;
    SingleLayout: TLayout;
    LocationSingleButton: TButton;
    SamplesSpinBox: TSpinBox;
    procedure LocationMultipleButtonClick(Sender: TObject);
    procedure LocationSingleButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
  private
    FLocationRefIndex: Integer;
    FLocationRefsConfig: TLocationRefsConfig;
    procedure DumpLocation(const ALocation: TLocationCoord2D; const AIndex: Integer = -1);
    procedure DumpLocations;
    procedure LoadConfig;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  DW.OSLog,
  DW.OSDevice,
  DW.Geodetic;

type
  TLocationCoord2DHelper = record helper for TLocationCoord2D
    function DisplayValue: string;
  end;

{ TLocationCoord2DHelper }

function TLocationCoord2DHelper.DisplayValue: string;
begin
  Result := Format('%.5f, %.5f', [Latitude, Longitude]);
end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  VersionLabel.Text := TOSDevice.GetPackageVersion;
  BottomLayout.Enabled := False;
  LoadConfig;
end;

procedure TForm1.LoadConfig;
var
  LRef: TLocationRef;
begin
  ListBox.Items.Clear;
  FLocationRefsConfig.Load;
  for LRef in FLocationRefsConfig.LocationRefs do
    ListBox.Items.Add(Format('%s (%s)', [LRef.Name, LRef.GetLocation.DisplayValue]));
  if ListBox.Items.Count > 0 then
  begin
    ListBox.ItemIndex := 0;
    BottomLayout.Enabled := True;
  end;
end;

procedure TForm1.LocationMultipleButtonClick(Sender: TObject);
begin
  FLocationRefIndex := ListBox.ItemIndex;
  LocationRequester.Request(TLocationRequestParams.CreateMultiple(SamplesSpinBox.Value.Exponent),
    procedure(const AState: TLocationResponseState; const ALocation: TLocationCoord2D)
    begin
      case AState of
        TLocationResponseState.AccessDenied:
          Sleep(0); // AKA Why you do that?
        TLocationResponseState.LocationDisabled:
          Sleep(0); // AKA Why you do that, part 2:
        TLocationResponseState.LocationTimeout:
          Sleep(0); // AKA Location sensor did not respond in time
        TLocationResponseState.Error:
          Sleep(0); // AKA Sum ting wong
        TLocationResponseState.OK:
          DumpLocations;
      end;
    end
  );
end;

procedure TForm1.LocationSingleButtonClick(Sender: TObject);
begin
  FLocationRefIndex := ListBox.ItemIndex;
  LocationRequester.Request(TLocationRequestParams.CreateSingle,
    procedure(const AState: TLocationResponseState; const ALocation: TLocationCoord2D)
    begin
      case AState of
        TLocationResponseState.AccessDenied:
          Sleep(0); // AKA Why you do that?
        TLocationResponseState.LocationDisabled:
          Sleep(0); // AKA Why you do that, part 2:
        TLocationResponseState.LocationTimeout:
          Sleep(0); // AKA Location sensor did not respond in time
        TLocationResponseState.Error:
          Sleep(0); // AKA Sum ting wong
        TLocationResponseState.OK:
          DumpLocation(ALocation);
      end;
    end
  );
end;

procedure TForm1.ClearButtonClick(Sender: TObject);
begin
  Memo.Lines.Clear;
end;

procedure TForm1.DumpLocation(const ALocation: TLocationCoord2D; const AIndex: Integer = -1);
var
  LRefLoc: TLocationCoord2D;
  LLocationCaption: string;
begin
  LRefLoc := FLocationRefsConfig.LocationRefs[FLocationRefIndex].GetLocation;
  if AIndex > -1 then
    LLocationCaption := Format('Loc #%d', [AIndex])
  else
    LLocationCaption := 'Loc';
  Memo.Lines.Add(Format('%s:  %s, Dist: %.2f', [LLocationCaption, ALocation.DisplayValue, TGeodetic.DistanceBetween(LRefLoc, ALocation)]));
end;

procedure TForm1.DumpLocations;
var
  I: Integer;
begin
  for I := 0 to Length(LocationRequester.Locations) - 1 do
    DumpLocation(LocationRequester.Locations[I], I);
end;

end.
