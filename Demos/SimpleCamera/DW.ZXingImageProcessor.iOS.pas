unit DW.ZXingImageProcessor.iOS;

interface

uses
  iOSapi.CoreGraphics,
  FMX.Graphics, FMX.Surfaces,
  DW.ImageProcessor, DW.BarcodeReader.Types;

type
  TBitmapConverter = class(TObject)
  private
    FContextRef: CGContextRef;
    FHeight: Cardinal;
    FSurface: TBitmapSurface;
    FWidth: Cardinal;
    function GetContextRef(const AWidth, AHeight: Cardinal): CGContextRef;
    procedure ReleaseContextRef;
  public
    constructor Create;
    destructor Destroy; override;
    function FromCGImageRef(const AImgRef: CGImageRef; const ABitmap: TBitmap): Boolean;
  end;

  TPlatformZXingImageProcessor = class(TInterfacedObject, IImageProcessor)
  private
    FBitmapConverter: TBitmapConverter;
    FProcessTime: TDateTime;
    FReader: IBarcodeReader;
    procedure ReceivedBarcode(const ABarcode: TBarcode; const AError: string);
    procedure Scan(const AImage: Pointer);
    procedure ScanBitmap(const ABitmap: TBitmap);
  public
    { IImageProcessor }
    procedure ProcessImage(const AImage: Pointer);
  public
    constructor Create(const AReader: IBarcodeReader);
    destructor Destroy; override;
  end;

implementation

uses
  DW.OSLog,
  System.Classes, System.SysUtils, System.Types, System.DateUtils,
  Macapi.Helpers,
  iOSapi.Foundation, iOSapi.UIKit,
  FMX.Types,
  ZXing.ScanManager, ZXing.BarcodeFormat, ZXing.ReadResult;

type
  TDWBarcodeFormat = DW.BarcodeReader.Types.TBarcodeFormat;

var
  GColorSpace: Pointer;

function ColorSpace: Pointer;
begin
  if GColorSpace = nil then
    GColorSpace := CGColorSpaceCreateDeviceRGB;
  Result := GColorSpace;
end;

function GetBarcodeFormat(const AFormat: TBarcodeFormat): TDWBarcodeFormat;
begin
  case AFormat of
    TBarcodeFormat.Auto:
      Result := TDWBarcodeFormat.All; // ?
    TBarcodeFormat.AZTEC:
      Result := TDWBarcodeFormat.Aztec;
    TBarcodeFormat.CODABAR:
      Result := TDWBarcodeFormat.Codabar;
    TBarcodeFormat.CODE_39:
      Result := TDWBarcodeFormat.Code39;
    TBarcodeFormat.CODE_93:
      Result := TDWBarcodeFormat.Code93;
    TBarcodeFormat.CODE_128:
      Result := TDWBarcodeFormat.Code128;
    TBarcodeFormat.DATA_MATRIX:
      Result := TDWBarcodeFormat.DataMatrix;
    TBarcodeFormat.EAN_8:
      Result := TDWBarcodeFormat.EAN8;
    TBarcodeFormat.EAN_13:
      Result := TDWBarcodeFormat.EAN13;
    TBarcodeFormat.ITF:
      Result := TDWBarcodeFormat.ITF;
    TBarcodeFormat.PDF_417:
      Result := TDWBarcodeFormat.PDF417;
    TBarcodeFormat.QR_CODE:
      Result := TDWBarcodeFormat.QR;
    TBarcodeFormat.UPC_A:
      Result := TDWBarcodeFormat.UPCA;
    TBarcodeFormat.UPC_E:
      Result := TDWBarcodeFormat.UPCE;
  else
    Result := TDWBarcodeFormat.Unknown;
  end;
end;

{ TBitmapConverter }

constructor TBitmapConverter.Create;
begin
  inherited;
  FSurface := TBitmapSurface.Create;
end;

destructor TBitmapConverter.Destroy;
begin
  ReleaseContextRef;
  FSurface.Free;
  inherited;
end;

function TBitmapConverter.FromCGImageRef(const AImgRef: CGImageRef; const ABitmap: TBitmap): Boolean;
var
  LContextRef: CGContextRef;
begin
  Result := False;
  LContextRef := GetContextRef(CGImageGetWidth(AImgRef), CGImageGetHeight(AImgRef));
  if LContextRef <> nil then
  begin
    CGContextDrawImage(LContextRef, CGRectFromRect(RectF(0, 0, FSurface.Width, FSurface.Height)), AImgRef);
    ABitmap.Assign(FSurface);
    Result := True;
  end;
end;

function TBitmapConverter.GetContextRef(const AWidth, AHeight: Cardinal): CGContextRef;
begin
  if (AWidth <> FWidth) or (AHeight <> FHeight) then
  begin
    FWidth := AWidth;
    FHeight := AHeight;
    ReleaseContextRef;
    FSurface.SetSize(FWidth, FHeight, TPixelFormat.BGRA); // BGRA  // RGBA
    FContextRef := CGBitmapContextCreate(FSurface.Bits, FWidth, FHeight, 8, FSurface.Width * 4, ColorSpace, kCGImageAlphaPremultipliedLast);
  end;
  Result := FContextRef;
end;

procedure TBitmapConverter.ReleaseContextRef;
begin
  if FContextRef <> nil then
    CGContextRelease(FContextRef);
end;

{ TPlatformZXingImageProcessor }

constructor TPlatformZXingImageProcessor.Create(const AReader: IBarcodeReader);
begin
  inherited Create;
  FReader := AReader;
  FBitmapConverter := TBitmapConverter.Create;
end;

destructor TPlatformZXingImageProcessor.Destroy;
begin
  FBitmapConverter.Free;
  inherited;
end;

procedure TPlatformZXingImageProcessor.ProcessImage(const AImage: Pointer);
begin
  if (MilliSecondsBetween(Now, FProcessTime) > 250) then
  begin
    FProcessTime := Now;
    TThread.CreateAnonymousThread(procedure begin Scan(AImage) end).Start;
  end;
end;

procedure TPlatformZXingImageProcessor.Scan(const AImage: Pointer);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    if FBitmapConverter.FromCGImageRef(AImage, LBitmap) then
      ScanBitmap(LBitmap);
  finally
    LBitmap.Free;
  end;
end;

procedure TPlatformZXingImageProcessor.ScanBitmap(const ABitmap: TBitmap);
var
  LScanManager: TScanManager;
  LResult: TReadResult;
  LBitmapTime: TDateTime;
begin
  LBitmapTime := Now;
  LScanManager := TScanManager.Create(TBarcodeFormat.Auto, nil);
  try
    LResult := LScanManager.Scan(ABitmap);
    if LResult <> nil then
    try
      TOSLog.d('TPlatformZXingImageProcessor scan completed in: %d msec', [MilliSecondsBetween(Now, FProcessTime)]);
      TOSLog.d('Bitmap conversion time was: %d msec', [MilliSecondsBetween(FProcessTime, LBitmapTime)]);
      ReceivedBarcode(TBarcode.Create(LResult.Text, GetBarcodeFormat(LResult.BarcodeFormat)), '');
    finally
      LResult.Free;
    end;
  finally
    LScanManager.Free;
  end;
end;

procedure TPlatformZXingImageProcessor.ReceivedBarcode(const ABarcode: TBarcode; const AError: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      FProcessTime := Now;
      FReader.ReceivedBarcodes([ABarcode], AError);
    end
  );
end;

end.
