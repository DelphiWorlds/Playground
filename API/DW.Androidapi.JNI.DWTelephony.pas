unit DW.Androidapi.JNI.DWTelephony;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Telephony;

type
  JDWUssdResponseCallback = interface;
  JDWUssdResponseCallbackDelegate = interface;

  JDWUssdResponseCallbackClass = interface(JTelephonyManager_UssdResponseCallbackClass)
    ['{7AE9B6B8-0134-4E81-A7DC-C5FE42EEDA63}']
    {class} function init(delegate: JDWUssdResponseCallbackDelegate): JDWUssdResponseCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWUssdResponseCallback')]
  JDWUssdResponseCallback = interface(JTelephonyManager_UssdResponseCallback)
    ['{32E41474-24FE-4501-BCF3-711ED0AAB0FD}']
  end;
  TJDWUssdResponseCallback = class(TJavaGenericImport<JDWUssdResponseCallbackClass, JDWUssdResponseCallback>) end;

  JDWUssdResponseCallbackDelegateClass = interface(IJavaClass)
    ['{E767E599-50C6-4503-B104-2E35C598E204}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWUssdResponseCallbackDelegate')]
  JDWUssdResponseCallbackDelegate = interface(IJavaInstance)
    ['{5786357E-EA06-4F87-8AAE-5A70630055A0}']
    procedure onReceiveUssdResponse(telephonyManager: JTelephonyManager; request: JString; response: JCharSequence); cdecl;
    procedure onReceiveUssdResponseFailed(telephonyManager: JTelephonyManager; request: JString; failureCode: Integer); cdecl;
  end;
  TJDWUssdResponseCallbackDelegate = class(TJavaGenericImport<JDWUssdResponseCallbackDelegateClass, JDWUssdResponseCallbackDelegate>) end;

implementation

end.
