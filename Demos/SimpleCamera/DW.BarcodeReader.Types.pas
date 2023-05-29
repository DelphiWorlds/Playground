unit DW.BarcodeReader.Types;

interface

type
  TBarcodeFormat = (All, Aztec, Codabar, Code128, Code39, Code93, DataMatrix, EAN13, EAN8, ITF, PDF417, QR, UPCA, UPCE, Unknown);
  TBarcodeFormats = set of TBarcodeFormat;

  TBarcode = record
    Format: TBarcodeFormat;
    Value: string;
    constructor Create(const AValue: string; const AFormat: TBarcodeFormat);
    function FormatDescription: string;
  end;

  TBarcodes = TArray<TBarcode>;

  IBarcodeReader = interface(IInterface)
    ['{FE35A6E6-D188-4158-A8FC-E7D526C6267E}']
    function GetFormats: TBarcodeFormats;
    procedure ReceivedBarcodes(const ABarcodes: TBarcodes; const AError: string);
    property Formats: TBarcodeFormats read GetFormats;
  end;

implementation

const
  cFormatDescriptions: array[TBarcodeFormat] of string = (
    'All', 'Aztec', 'Codabar', 'Code-128', 'Code-39', 'Code-93', 'DataMatrix', 'EAN-13', 'EAN-8', 'ITF', 'PDF-417', 'QR Code',
    'UPC-A', 'UPC-E', 'Unknown'
  );

{ TBarcode }

constructor TBarcode.Create(const AValue: string; const AFormat: TBarcodeFormat);
begin
  Format := AFormat;
  Value := AValue;
end;

function TBarcode.FormatDescription: string;
begin
  Result := cFormatDescriptions[Format];
end;

end.
