program DelphiMVCwithDI;

{$APPTYPE CONSOLE}

uses
  {$IF defined(DEBUG) and defined(MSWINDOWS)}
  FastMM4,
  {$ENDIF }
  System.SysUtils,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.REPLCommandsHandlerU,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdContext,
  IdHTTPWebBrokerBridge,
  Spring.Container,
  System.IOUtils,
  Application.Controllers.Customer in 'Source\Application\Controllers\Application.Controllers.Customer.pas',
  Application.Services.Customer in 'Source\Application\Services\Application.Services.Customer.pas',
  Application.WebModule in 'Source\Application\Application.WebModule.pas' {AppWebModule: TWebModule},
  Domain.Entities.Customer in 'Source\Domain\Entities\Domain.Entities.Customer.pas',
  Domain.Interfaces.Customer in 'Source\Domain\Interfaces\Domain.Interfaces.Customer.pas',
  Infrastructure.DbContext.Interfaces in 'Source\Infrastructure\Infrastructure.DbContext.Interfaces.pas',
  Infrastructure.DbContext in 'Source\Infrastructure\Infrastructure.DbContext.pas',
  Infrastructure.Repositories.Customer in 'Source\Infrastructure\Repositories\Infrastructure.Repositories.Customer.pas',
  Application.DependencyInjection in 'Source\Application\Application.DependencyInjection.pas',
  Application.DependencyInjection.SingletonPerScope in 'Source\Application\Application.DependencyInjection.SingletonPerScope.pas',
  Application.Middlewares.SingletonPerScopeFinalizer in 'Source\Application\Middlewares\Application.Middlewares.SingletonPerScopeFinalizer.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
  LCustomHandler: TMVCCustomREPLCommandsHandler;
  LCmd: string;
begin
{$IF defined(DEBUG)}
  if TDirectory.Exists(AppPath + 'logs') then
    TDirectory.Delete(AppPath + 'logs', True);
{$ENDIF}

  Writeln('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  LCmd := 'start';
  if ParamCount >= 1 then
    LCmd := ParamStr(1);

  LCustomHandler := function(const Value: String; const Server: TIdHTTPWebBrokerBridge; out Handled: Boolean): THandleCommandResult
    begin
      Handled := False;
      Result := THandleCommandResult.Unknown;
    end;

  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;
    LServer.KeepAlive := True;

    { more info about MaxConnections
      http://ww2.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=index.html }
    LServer.MaxConnections := 0;

    { more info about ListenQueue
      http://ww2.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=index.html }
    LServer.ListenQueue := 200;

    WriteLn('Write "quit" or "exit" to shutdown the server');
    repeat
      if LCmd.IsEmpty then
      begin
        Write('-> ');
        ReadLn(LCmd)
      end;
      try
        case HandleCommand(LCmd.ToLower, LServer, LCustomHandler) of
          THandleCommandResult.Continue:
            begin
              Continue;
            end;
          THandleCommandResult.Break:
            begin
              Break;
            end;
          THandleCommandResult.Unknown:
            begin
              REPLEmit('Unknown command: ' + LCmd);
            end;
        end;
      finally
        LCmd := '';
      end;
    until False;

  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;

    // Spring container configurations
    RegisterDependencies;
    GlobalContainer.Build;

    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
