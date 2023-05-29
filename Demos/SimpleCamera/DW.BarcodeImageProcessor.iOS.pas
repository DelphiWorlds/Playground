unit DW.BarcodeImageProcessor.iOS;

interface

uses
  iOSapi.Foundation, iOSapi.UIKit,
  DW.ImageProcessor, DW.iOSapi.MLKitBarcodeScanning, DW.BarcodeReader.Types;

type
  IBarcodeWorker = interface(IInterface)
    ['{7D2BC11E-0607-4B55-935F-54C4D6A6954B}']
    procedure Scan;
  end;

  TPlatformBarcodeImageProcessor = class(TInterfacedObject, IImageProcessor)
  private
    FProcessTime: TDateTime;
    FReader: IBarcodeReader;
    FScanner: MLKBarcodeScanner;
    FWorker: IBarcodeWorker;
    function GetCodeFormats: MLKBarcodeFormat;
    function GetBarcodeFormat(const AFormat: MLKBarcodeFormat): TBarcodeFormat;
    procedure DoProcessImage(const AImage: Pointer);
    procedure ScannerProcessImageCompletionHandler(barcodes: NSArray; error: NSError);
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
  Macapi.Helpers,
  DW.iOSapi.MLKitVision;

const
  cFormatValues: array[TBarcodeFormat] of MLKBarcodeFormat = (
    MLKBarcodeFormatAll,
    MLKBarcodeFormatAztec,
    MLKBarcodeFormatCodaBar,
    MLKBarcodeFormatCode128,
    MLKBarcodeFormatCode39,
    MLKBarcodeFormatCode93,
    MLKBarcodeFormatDataMatrix,
    MLKBarcodeFormatEAN8,
    MLKBarcodeFormatEAN13,
    MLKBarcodeFormatITF,
    MLKBarcodeFormatPDF417,
    MLKBarcodeFormatQRCode,
    MLKBarcodeFormatUPCA,
    MLKBarcodeFormatUPCE,
    MLKBarcodeFormatUnknown
  );

type
  TPlatformBarcodeWorker = class(TInterfacedObject, IBarcodeWorker)
  private
    FScanner: MLKBarcodeScanner;
    FProcessor: TPlatformBarcodeImageProcessor;
    FImage: MLKVisionImage;
  public
    { IBarcodeWorker }
    procedure Scan;
  public
    constructor Create(const AProcessor: TPlatformBarcodeImageProcessor; const AImage: Pointer);
  end;

{ TPlatformBarcodeWorker }

constructor TPlatformBarcodeWorker.Create(const AProcessor: TPlatformBarcodeImageProcessor; const AImage: Pointer);
var
  LOptions: MLKBarcodeScannerOptions;
  LUIImage: UIImage;
begin
  inherited Create;
  FProcessor := AProcessor;
  LUIImage := TUIImage.Wrap(TUIImage.OCClass.imageWithCGImage(AImage));
  FImage := TMLKVisionImage.Wrap(TMLKVisionImage.Alloc.initWithImage(LUIImage));
  LOptions := TMLKBarcodeScannerOptions.Create;
  LOptions := TMLKBarcodeScannerOptions.Wrap(LOptions.initWithFormats(AProcessor.GetCodeFormats));
  FScanner := TMLKBarcodeScanner.Wrap(TMLKBarcodeScanner.OCClass.barcodeScannerWithOptions(LOptions));
end;

procedure TPlatformBarcodeWorker.Scan;
begin
  FScanner.processImage(FImage, FProcessor.ScannerProcessImageCompletionHandler);
end;

{ TPlatformBarcodeImageProcessor }

constructor TPlatformBarcodeImageProcessor.Create(const AReader: IBarcodeReader);
begin
  inherited Create;
  FReader := AReader;
end;

function TPlatformBarcodeImageProcessor.GetBarcodeFormat(const AFormat: MLKBarcodeFormat): TBarcodeFormat;
var
  LBarcodeFormat: TBarcodeFormat;
begin
  Result := TBarcodeFormat.Unknown;
  for LBarcodeFormat := Succ(Low(TBarcodeFormat)) to Pred(High(TBarcodeFormat)) do
  begin
    if AFormat = cFormatValues[LBarcodeFormat] then
    begin
      Result := LBarcodeFormat;
      Break;
    end;
  end;
end;

function TPlatformBarcodeImageProcessor.GetCodeFormats: MLKBarcodeFormat;
var
  LBarcodeFormat: TBarcodeFormat;
begin
  Result := MLKBarcodeFormatUnknown;
  if not (TBarcodeFormat.All in FReader.Formats) then
  begin
    for LBarcodeFormat := Succ(Low(TBarcodeFormat)) to Pred(High(TBarcodeFormat)) do
    begin
      if LBarcodeFormat in FReader.Formats then
        Result := Result or cFormatValues[LBarcodeFormat];
    end;
  end
  else
    Result := MLKBarcodeFormatAll;
end;

procedure TPlatformBarcodeImageProcessor.DoProcessImage(const AImage: Pointer);
begin
  FWorker := TPlatformBarcodeWorker.Create(Self, AImage);
end;

procedure TPlatformBarcodeImageProcessor.ProcessImage(const AImage: Pointer);
begin
  // May need to start this in the main thread
  // TThread.Synchronize(nil, procedure begin DoProcessImage(AImage) end);
  // if MilliSecondsBetween(Now, FProcessTime) > 100 then
  if (MilliSecondsBetween(Now, FProcessTime) > 250) and (FWorker = nil) then
  begin
    FProcessTime := Now;
    FWorker := TPlatformBarcodeWorker.Create(Self, AImage);
    TThread.CreateAnonymousThread(procedure begin FWorker.Scan end).Start;
  end;
end;

procedure TPlatformBarcodeImageProcessor.ScannerProcessImageCompletionHandler(barcodes: NSArray; error: NSError);
var
  LMLKBarcode: MLKBarcode;
  LBarcode: TBarcode;
  LBarcodes: TBarcodes;
  I: Integer;
  LError: string;
begin
  if (barcodes <> nil) and (barcodes.count > 0) then
  begin
    for I := 0 to barcodes.count - 1 do
    begin
      LMLKBarcode := TMLKBarcode.Wrap(barcodes.objectAtIndex(I));
      LBarcode.Value := NSStrToStr(LMLKBarcode.displayValue);
      LBarcode.Format := GetBarcodeFormat(LMLKBarcode.format);
      LBarcodes := LBarcodes + [LBarcode];
    end;
    TOSLog.d('TPlatformBarcodeImageProcessor scan completed in %d msec', [MilliSecondsBetween(Now, FProcessTime)]);
  end;
  if error <> nil then
    LError := NSStrToStr(error.localizedDescription)
  else
    LError := '';
  TThread.Synchronize(nil,
    procedure
    begin
      if (Length(LBarcodes) > 0) or not LError.IsEmpty then
        FReader.ReceivedBarcodes(LBarcodes, LError);
      FWorker := nil;
      FProcessTime := Now;
    end
  );
end;

end.
