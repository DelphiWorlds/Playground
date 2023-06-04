unit DW.ZXingImageProcessor.Android;

interface

uses
  System.Classes,
  Androidapi.JNI.Media,
  FMX.Graphics,
  DW.ImageProcessor, DW.BarcodeReader.Types;

type
  TPlatformZXingImageProcessor = class(TInterfacedObject, IImageProcessor)
  private
    FProcessTime: TDateTime;
    FReader: IBarcodeReader;
    procedure ReceivedBarcode(const ABarcode: TBarcode; const AError: string);
    procedure ScanBitmap(const ABitmap: TBitmap);
    procedure ScanImage(const AImage: JImage);
    procedure ScanStream(const AStream: TStream);
  public
    { IImageProcessor }
    procedure ProcessImage(const AImage: Pointer);
  public
    constructor Create(const AReader: IBarcodeReader);
  end;

implementation

uses
  DW.OSLog,
  System.SysUtils, System.Types, System.DateUtils,
  Androidapi.JNI.JavaTypes, Androidapi.JNIBridge, Androidapi.Helpers, Androidapi.JNI,
  ZXing.ScanManager, ZXing.BarcodeFormat, ZXing.ReadResult;

type
  TDWBarcodeFormat = DW.BarcodeReader.Types.TBarcodeFormat;

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

{ TPlatformZXingImageProcessor }

constructor TPlatformZXingImageProcessor.Create(const AReader: IBarcodeReader);
begin
  inherited Create;
  FReader := AReader;
end;

procedure TPlatformZXingImageProcessor.ProcessImage(const AImage: Pointer);
begin
  if (MilliSecondsBetween(Now, FProcessTime) > 250) then
  begin
    FProcessTime := Now;
    ScanImage(TJImage.Wrap(AImage));
  end;
end;

procedure TPlatformZXingImageProcessor.ScanImage(const AImage: JImage);
var
  LBuffer: JByteBuffer;
  LPlanes: TJavaObjectArray<JImage_Plane>;
  LBytes: TJavaArray<Byte>;
  LStream: TBytesStream;
begin
  LPlanes := AImage.getPlanes;
  try
    LBuffer := LPlanes.Items[0].getBuffer; // <---- This will work only if the format is JPEG
    LBytes := TJavaArray<Byte>.Create(LBuffer.capacity);
    try
      LBuffer.get(LBytes);
      LStream := TBytesStream.Create(TAndroidHelper.TJavaArrayToTBytes(LBytes));
      try
        // TThread.Synchronize(nil , procedure begin ScanStream(LStream); end);
        ScanStream(LStream);
      finally
        LStream.Free;
      end;
    finally
      LBytes.Free;
    end;
  finally
    LPlanes.Free;
  end;
end;

procedure TPlatformZXingImageProcessor.ScanStream(const AStream: TStream);
var
  LBitmap: TBitmap;
  LLoadTime: TDateTime;
begin
  LLoadTime := Now;
  LBitmap := TBitmap.Create;
  try
    LBitmap.LoadFromStream(AStream);
    TOSLog.d('Bitmap load time was: %d msec', [MilliSecondsBetween(Now, LLoadTime)]);
    ScanBitmap(LBitmap);
  finally
    LBitmap.Free;
  end;
end;

procedure TPlatformZXingImageProcessor.ScanBitmap(const ABitmap: TBitmap);
var
  LScanManager: TScanManager;
  LResult: TReadResult;
  LBitmapTime, LScanTime: TDateTime;
begin
  LBitmapTime := Now;
  LScanManager := TScanManager.Create(TBarcodeFormat.Auto, nil);
  try
    LResult := LScanManager.Scan(ABitmap);
    LScanTime := Now;
    if LResult <> nil then
    try
      ReceivedBarcode(TBarcode.Create(LResult.Text, GetBarcodeFormat(LResult.BarcodeFormat)), '');
    finally
      LResult.Free;
    end;
    TOSLog.d('TPlatformZXingImageProcessor scan completed in: %d msec', [MilliSecondsBetween(LScanTime, FProcessTime)]);
    TOSLog.d('Bitmap conversion time was: %d msec', [MilliSecondsBetween(FProcessTime, LBitmapTime)]);
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
