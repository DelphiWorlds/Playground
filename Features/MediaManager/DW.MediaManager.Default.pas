unit DW.MediaManager.Default;

{$I DW.GlobalDefines.inc}

interface

uses
  // DW
  DW.MediaManager;

type
  /// <summary>
  ///   Placeholder Photos manager class for unsupported platforms.
  /// </summary>
  TPlatformMediaManager = class(TCustomPlatformMediaManager);

  /// <summary>
  ///   Placeholder Photos collection class for unsupported platforms.
  /// </summary>
  TPlatformMediaCollections = class(TMediaCollections);

implementation

end.
