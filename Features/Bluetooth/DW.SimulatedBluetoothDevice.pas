unit DW.SimulatedBluetoothDevice;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

const
  cUUIDWith16Bit = '0000%s-0000-1000-8000-00805F9B34FB';

type
  TCustomPlatformSimulatedBluetoothDevice = class(TObject)
  private
    procedure SetIsActive(const Value: Boolean);
  protected
    FIsAdvertising: Boolean;
    FIsActive: Boolean;
    procedure ActiveChanging(const Value: Boolean); virtual;
    procedure ServicesAdded; virtual;
    procedure StartAdvertising; virtual;
    procedure StartServer; virtual;
    procedure StopAdvertising; virtual;
    procedure StopServer; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property IsActive: Boolean read FIsActive write SetIsActive;
  end;

function StringToServiceUUID(const AValue: string): string;

implementation

uses
  // RTL
  System.SysUtils;

function StringToServiceUUID(const AValue: string): string;
begin
  Result := AValue;
  if Length(Result) = 4 then
    Result := Format(cUUIDWith16Bit, [Result]);
end;

{ TCustomPlatformSimulatedBluetoothDevice }

constructor TCustomPlatformSimulatedBluetoothDevice.Create;
begin
  inherited;
  //
end;

destructor TCustomPlatformSimulatedBluetoothDevice.Destroy;
begin
  StopServer;
  inherited;
end;

procedure TCustomPlatformSimulatedBluetoothDevice.ServicesAdded;
begin
  StartAdvertising;
end;

procedure TCustomPlatformSimulatedBluetoothDevice.SetIsActive(const Value: Boolean);
begin
  if FIsActive <> Value then
    ActiveChanging(Value);
end;

procedure TCustomPlatformSimulatedBluetoothDevice.ActiveChanging(const Value: Boolean);
begin
  FIsActive := Value;
end;

procedure TCustomPlatformSimulatedBluetoothDevice.StartAdvertising;
begin
  //
end;

procedure TCustomPlatformSimulatedBluetoothDevice.StartServer;
begin
  //
end;

procedure TCustomPlatformSimulatedBluetoothDevice.StopAdvertising;
begin
  //
end;

procedure TCustomPlatformSimulatedBluetoothDevice.StopServer;
begin
  //
end;

end.
