unit DW.BarcodeImageProcessor.Android;

interface

uses
  Androidapi.JNI.Media,
  DW.ImageProcessor, DW.BarcodeReader.Types;

type
  TPlatformBarcodeImageProcessor = class(TInterfacedObject, IImageProcessor)
  private
    FProcessTime: TDateTime;
    FReader: IBarcodeReader;
    procedure DoProcessImage(const AImage: JImage);
    function GetBarcodeFormat(const AFormat: Integer): TBarcodeFormat;
    function GetFormatsValue(const AFormats: TBarcodeFormats): Integer;
  public
    { IImageProcessor }
    procedure ProcessImage(const AImage: Pointer);
  public
    constructor Create(const AReader: IBarcodeReader);
  end;

implementation

uses
  DW.OSLog,
  System.Classes, System.SysUtils,
  System.DateUtils,
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Util, Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText,
  DW.Androidapi.JNI.VisionBarcode, DW.Android.Helpers;

{ TPlatformBarcodeImageProcessor }

constructor TPlatformBarcodeImageProcessor.Create(const AReader: IBarcodeReader);
begin
  inherited Create;
  FReader := AReader;
end;

function TPlatformBarcodeImageProcessor.GetBarcodeFormat(const AFormat: Integer): TBarcodeFormat;
begin
  Result := TBarcodeFormat.Unknown;
  if AFormat = TJBarcode.JavaClass.AZTEC then
    Result := TBarcodeFormat.Aztec
  else if AFormat = TJBarcode.JavaClass.CODABAR then
    Result := TBarcodeFormat.Codabar
  else if AFormat = TJBarcode.JavaClass.CODE_128 then
    Result := TBarcodeFormat.Code128
  else if AFormat = TJBarcode.JavaClass.CODE_39 then
    Result := TBarcodeFormat.Code39
  else if AFormat = TJBarcode.JavaClass.CODE_93 then
    Result := TBarcodeFormat.Code93
  else if AFormat = TJBarcode.JavaClass.DATA_MATRIX then
    Result := TBarcodeFormat.DataMatrix
  else if AFormat = TJBarcode.JavaClass.EAN_13 then
    Result := TBarcodeFormat.EAN13
  else if AFormat = TJBarcode.JavaClass.EAN_8 then
    Result := TBarcodeFormat.EAN8
  else if AFormat = TJBarcode.JavaClass.ITF then
    Result := TBarcodeFormat.ITF
  else if AFormat = TJBarcode.JavaClass.PDF417 then
    Result := TBarcodeFormat.PDF417
  else if AFormat = TJBarcode.JavaClass.QR_CODE then
    Result := TBarcodeFormat.QR
  else if AFormat = TJBarcode.JavaClass.UPC_A then
    Result := TBarcodeFormat.UPCA
  else if AFormat = TJBarcode.JavaClass.UPC_E then
    Result := TBarcodeFormat.UPCE;
end;

function TPlatformBarcodeImageProcessor.GetFormatsValue(const AFormats: TBarcodeFormats): Integer;
var
  LIsAll: Boolean;
begin
  Result := 0;
  LIsAll := TBarcodeFormat.All in AFormats;
  if LIsAll or (TBarcodeFormat.Aztec in AFormats) then
    Result := Result or TJBarcode.JavaClass.AZTEC;
  if LIsAll or (TBarcodeFormat.Codabar in AFormats) then
    Result := Result or TJBarcode.JavaClass.CODABAR;
  if LIsAll or (TBarcodeFormat.Code128 in AFormats) then
    Result := Result or TJBarcode.JavaClass.CODE_128;
  if LIsAll or (TBarcodeFormat.Code39 in AFormats) then
    Result := Result or TJBarcode.JavaClass.CODE_39;
  if LIsAll or (TBarcodeFormat.Code93 in AFormats) then
    Result := Result or TJBarcode.JavaClass.CODE_93;
  if LIsAll or (TBarcodeFormat.DataMatrix in AFormats) then
    Result := Result or TJBarcode.JavaClass.DATA_MATRIX;
  if LIsAll or (TBarcodeFormat.EAN13 in AFormats) then
    Result := Result or TJBarcode.JavaClass.EAN_13;
  if LIsAll or (TBarcodeFormat.EAN8 in AFormats) then
    Result := Result or TJBarcode.JavaClass.EAN_8;
  if LIsAll or (TBarcodeFormat.ITF in AFormats) then
    Result := Result or TJBarcode.JavaClass.ITF;
  if LIsAll or (TBarcodeFormat.PDF417 in AFormats) then
    Result := Result or TJBarcode.JavaClass.PDF417;
  if LIsAll or (TBarcodeFormat.QR in AFormats) then
    Result := Result or TJBarcode.JavaClass.QR_CODE;
  if LIsAll or (TBarcodeFormat.UPCA in AFormats) then
    Result := Result or TJBarcode.JavaClass.UPC_A;
  if LIsAll or (TBarcodeFormat.UPCE in AFormats) then
    Result := Result or TJBarcode.JavaClass.UPC_E;
end;

procedure TPlatformBarcodeImageProcessor.DoProcessImage(const AImage: JImage);
var
  LDetectorBuilder: JBarcodeDetector_Builder;
  LFrameBuilder: JFrame_Builder;
  LResults: JSparseArray;
  LJBarcode: JBarcode;
  I: Integer;
  LBarcode: TBarcode;
  LBarcodes: TBarcodes;
begin
  LDetectorBuilder := TJBarcodeDetector_Builder.JavaClass.init(TAndroidHelper.Context);
  LDetectorBuilder.setBarcodeFormats(GetFormatsValue([TBarcodeFormat.All]));
  LFrameBuilder := TJFrame_Builder.Create;
  LFrameBuilder.setBitmap(TJImageHelper.JImageToJBitmap(AImage));
  LResults := LDetectorBuilder.build.detect(LFrameBuilder.build);
  for I := 0 to LResults.size - 1 do
  begin
    LJBarcode := TJBarcode.Wrap(TAndroidHelper.JObjectToID(LResults.get(LResults.keyAt(0))));
    LBarcode.Value := JStringToString(LJBarcode.displayValue);
    LBarcode.Format := GetBarcodeFormat(LJBarcode.format);
    LBarcodes := LBarcodes + [LBarcode];
  end;
  TOSLog.d('TPlatformBarcodeImageProcessor scan completed in %d msec', [MilliSecondsBetween(Now, FProcessTime)]);
  TThread.Synchronize(nil,
    procedure
    begin
      FReader.ReceivedBarcodes(LBarcodes, '');
    end
  );
end;

procedure TPlatformBarcodeImageProcessor.ProcessImage(const AImage: Pointer);
var
  LImage: JImage;
begin
  if (MilliSecondsBetween(Now, FProcessTime) > 250) then
  begin
    LImage := TJImage.Wrap(AImage);
    FProcessTime := Now;
    DoProcessImage(LImage);
  end;
end;

end.
