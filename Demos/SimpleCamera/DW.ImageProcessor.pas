unit DW.ImageProcessor;

interface

type
  IImageProcessor = interface(IInterface)
    ['{7621E6F2-1F77-4D45-87D5-32FD0C7CD3B4}']
    procedure ProcessImage(const AImage: Pointer);
  end;

implementation


end.
