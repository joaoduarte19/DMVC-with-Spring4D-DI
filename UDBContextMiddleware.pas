unit UDBContextMiddleware;

interface

uses
  MVCFramework,
  UDBContext.Interfaces;

type
  TDBContextMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    FGuid: TGuid;
    FDBContext: IDBContext;
  public
    constructor Create;
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string; const AActionName: string; var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string; const AActionName: string; const AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  end;


implementation

uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti,
  System.Classes,
  Spring.Container,
  MVCFramework.Logger,
  UDBContextLifetime;

{ TDBContextMiddleware }

constructor TDBContextMiddleware.Create;
begin
  FGuid := TGuid.NewGuid;
end;

procedure TDBContextMiddleware.OnAfterControllerAction(AContext: TWebContext; const AControllerQualifiedClassName, AActionName: string; const AHandled: Boolean);
begin
 //
end;

procedure TDBContextMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
begin
  FDBContext := nil;

  DBContextLifetimeManager.Release(TValue.Empty);
  Log.Info('%s - TDBContextMiddleware.OnAfterRouting', [FGuid.ToString], 'life_cicle');
end;

procedure TDBContextMiddleware.OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName, AActionName: string; var AHandled: Boolean);
begin
  //
end;

procedure TDBContextMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
begin
  Log.Info('%s - TDBContextMiddleware.OnBeforeRouting', [FGuid.ToString], 'life_cicle');

  FDBContext := GlobalContainer.Resolve<IDBContext>;
  // Configs;
end;

end.
