unit DW.UssdRequester;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Telephony,
  // DW
  DW.Androidapi.JNI.DWTelephony;

type
  TUssdRequester = class;

  TUssdResponseCallbackDelegate = class(TJavaLocal, JDWUssdResponseCallbackDelegate)
  private
    FCallback: JTelephonyManager_UssdResponseCallback;
    FRequester: TUssdRequester;
  public
    { JDWUssdResponseCallbackDelegate }
    procedure onReceiveUssdResponse(telephonyManager: JTelephonyManager; request: JString; response: JCharSequence); cdecl;
    procedure onReceiveUssdResponseFailed(telephonyManager: JTelephonyManager; request: JString; failureCode: Integer); cdecl;
  public
    constructor Create(const ARequester: TUssdRequester);
    property Callback: JTelephonyManager_UssdResponseCallback read FCallback;
  end;

  TUssdFailure = (Unknown, None, ReturnFailure, ServiceUnavailable);

  TUssdResponseEvent = procedure(Sender: TObject; const Request, Response: string; const Failure: TUssdFailure) of object;

  TUssdRequester = class(TObject)
  private
    FTelephonyManager: JTelephonyManager;
    FUssdResponseCallbackDelegate: TUssdResponseCallbackDelegate;
    FOnUssdResponse: TUssdResponseEvent;
    procedure DoUssdResponse(const ARequest, AResponse: string; const AFailure: TUssdFailure);
  protected
    procedure ReceiveUssdResponse(telephonyManager: JTelephonyManager; request: JString; response: JCharSequence);
    procedure ReceiveUssdResponseFailed(telephonyManager: JTelephonyManager; request: JString; failureCode: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SendUssdRequest(const ARequest: string);
    property OnUssdResponse: TUssdResponseEvent read FOnUssdResponse write FOnUssdResponse;
  end;

implementation

uses
  // Android
  Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Os;

{ TUssdResponseCallbackDelegate }

constructor TUssdResponseCallbackDelegate.Create(const ARequester: TUssdRequester);
begin
  inherited Create;
  FRequester := ARequester;
  FCallback := TJDWUssdResponseCallback.JavaClass.init(Self);
end;

procedure TUssdResponseCallbackDelegate.onReceiveUssdResponse(telephonyManager: JTelephonyManager; request: JString; response: JCharSequence);
begin
  FRequester.ReceiveUssdResponse(telephonyManager, request, response);
end;

procedure TUssdResponseCallbackDelegate.onReceiveUssdResponseFailed(telephonyManager: JTelephonyManager; request: JString; failureCode: Integer);
begin
  FRequester.ReceiveUssdResponseFailed(telephonyManager, request, failureCode);
end;

{ TUssdRequester }

constructor TUssdRequester.Create;
begin
  inherited;
  FUssdResponseCallbackDelegate := TUssdResponseCallbackDelegate.Create(Self);
  FTelephonyManager := TJTelephonyManager.Wrap(TAndroidHelper.JObjectToID(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.TELEPHONY_SERVICE)));
end;

destructor TUssdRequester.Destroy;
begin
  FUssdResponseCallbackDelegate.Free;
  inherited;
end;

procedure TUssdRequester.DoUssdResponse(const ARequest, AResponse: string; const AFailure: TUssdFailure);
begin
  if Assigned(FOnUssdResponse) then
    FOnUssdResponse(Self, ARequest, AResponse, AFailure);
end;

procedure TUssdRequester.ReceiveUssdResponse(telephonyManager: JTelephonyManager; request: JString; response: JCharSequence);
begin
  DoUssdResponse(JStringToString(request), JCharSequenceToStr(response), TUssdFailure.None);
end;

procedure TUssdRequester.ReceiveUssdResponseFailed(telephonyManager: JTelephonyManager; request: JString; failureCode: Integer);
var
  LFailure: TUssdFailure;
begin
  if failureCode = TJTelephonyManager.JavaClass.USSD_ERROR_SERVICE_UNAVAIL then
    LFailure := TUssdFailure.ServiceUnavailable
  else if failureCode = TJTelephonyManager.JavaClass.USSD_RETURN_FAILURE then
    LFailure := TUssdFailure.ReturnFailure
  else
    LFailure := TUssdFailure.Unknown;
  DoUssdResponse(JStringToString(request), '', LFailure);
end;

procedure TUssdRequester.SendUssdRequest(const ARequest: string);
begin
  FTelephonyManager.sendUssdRequest(StringToJString(ARequest), FUssdResponseCallbackDelegate.Callback, TJHandler.JavaClass.init);
end;

end.
