unit DW.Beacons.iOS;

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

uses
  DW.Beacons;

type
  TPlatformBeacons = class(TCustomPlatformBeacons)
  private
    //
  protected
    //
  public
    constructor Create(const ABeacons: TBeacons); override;
    destructor Destroy; override;
  end;

implementation

{ TPlatformBeacons }

constructor TPlatformBeacons.Create(const ABeacons: TBeacons);
begin
  inherited;
  //
end;

destructor TPlatformBeacons.Destroy;
begin
  //
  inherited;
end;

end.
