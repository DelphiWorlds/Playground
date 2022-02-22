unit DW.Template.iOS;

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
  DW.Template;

type
  TPlatformTemplate = class(TCustomPlatformTemplate)
  private
    //
  protected
    //
  public
    constructor Create(const ATemplate: TTemplate); override;
    destructor Destroy; override;
  end;

implementation

{ TPlatformTemplate }

constructor TPlatformTemplate.Create(const ATemplate: TTemplate);
begin
  inherited;
  //
end;

destructor TPlatformTemplate.Destroy;
begin
  //
  inherited;
end;

end.
