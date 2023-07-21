unit Application.WebModule;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TAppWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FEngine: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TAppWebModule;

implementation

{$R *.dfm}

uses
  Spring.Container,
  System.IOUtils,
  MVCFramework.Commons,
  MVCFramework.Middleware.StaticFiles, 
  MVCFramework.Middleware.Compression,
  MVCFramework.Controllers.Register,
  Application.Middlewares.SingletonPerScopeFinalizer;

procedure TAppWebModule.WebModuleCreate(Sender: TObject);
begin
  FEngine := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      // session timeout (0 means session cookie)
      Config[TMVCConfigKey.SessionTimeout] := '0';
      //default content-type
      Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
      //default content charset
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      //unhandled actions are permitted?
      Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
      //enables or not system controllers loading (available only from localhost requests)
      Config[TMVCConfigKey.LoadSystemControllers] := 'false';
      //Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := 'true';
      //Enable X-Powered-By Header in response
      Config[TMVCConfigKey.ExposeXPoweredBy] := 'true';
      // Max request size in bytes
      Config[TMVCConfigKey.MaxRequestSize] := IntToStr(TMVCConstants.DEFAULT_MAX_REQUEST_SIZE);
    end);
  TControllersRegister.Instance.AddControllersInEngine(FEngine);


  FEngine.AddMiddleware(TMVCCompressionMiddleware.Create);
  FEngine.AddMiddleware(TSingletonPerScopeFinalizerMiddleware.Create);
end;

procedure TAppWebModule.WebModuleDestroy(Sender: TObject);
begin
  FEngine.Free;
end;

end.
