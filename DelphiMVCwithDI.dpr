program DelphiMVCwithDI;

{$APPTYPE CONSOLE}

uses
  FastMM4,
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
  UCustomer.Controller in 'Source\UCustomer.Controller.pas',
  UWebModule in 'Source\UWebModule.pas' {ServerWebModule: TWebModule},
  UCustomer.Interfaces in 'Source\UCustomer.Interfaces.pas',
  UCustomer.Entity in 'Source\UCustomer.Entity.pas',
  UCustomer.Service in 'Source\UCustomer.Service.pas',
  UCustomer.Repository in 'Source\UCustomer.Repository.pas',
  UDBContext.Interfaces in 'Source\UDBContext.Interfaces.pas',
  UDBContext.FireDAC in 'Source\UDBContext.FireDAC.pas',
  UDBContextMiddleware in 'Source\UDBContextMiddleware.pas',
  UDBContextLifetime in 'Source\UDBContextLifetime.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
  LCustomHandler: TMVCCustomREPLCommandsHandler;
  LCmd: string;
begin
  TDirectory.Delete('.\logs', True);

  Writeln('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  LCmd := 'start';
  if ParamCount >= 1 then
    LCmd := ParamStr(1);

  LCustomHandler := function(const Value: String; const Server: TIdHTTPWebBrokerBridge; out Handled: Boolean): THandleCommandResult
    begin
      Handled := False;
      Result := THandleCommandResult.Unknown;

      // Write here your custom command for the REPL using the following form...
      // ***
      // Handled := False;
      // if (Value = 'apiversion') then
      // begin
      // REPLEmit('Print my API version number');
      // Result := THandleCommandResult.Continue;
      // Handled := True;
      // end
      // else if (Value = 'datetime') then
      // begin
      // REPLEmit(DateTimeToStr(Now));
      // Result := THandleCommandResult.Continue;
      // Handled := True;
      // end;
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

    GlobalContainer.Build;

    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
