unit Application.Middlewares.SingletonPerScopeFinalizer;

interface

uses
  MVCFramework;

type
  TSingletonPerScopeFinalizerMiddleware = class(TInterfacedObject, IMVCMiddleware)
  public
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string; const AActionName: string; var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string; const AActionName: string; const AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  end;

implementation

uses
  Spring,
  Spring.Collections,
  Spring.Container.Core,
  Application.DependencyInjection.SingletonPerScope;

{ TSingletonPerScopeFinalizerMiddleware }

procedure TSingletonPerScopeFinalizerMiddleware.OnAfterControllerAction(AContext: TWebContext; const AControllerQualifiedClassName, AActionName: string; const AHandled: Boolean);
begin
  //
end;

procedure TSingletonPerScopeFinalizerMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
begin
  TSingletonPerScopeLifetimeManagerRepository.Instances.ForEach(
    procedure(const AItem: TPair<string, ILifetimeManager>)
    begin
      AItem.Value.Release(TValue.Empty);
    end
  );
end;

procedure TSingletonPerScopeFinalizerMiddleware.OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName, AActionName: string; var AHandled: Boolean);
begin
//
end;

procedure TSingletonPerScopeFinalizerMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
begin
 //
end;


end.
