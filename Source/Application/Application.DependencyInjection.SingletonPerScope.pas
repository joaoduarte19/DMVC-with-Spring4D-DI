unit Application.DependencyInjection.SingletonPerScope;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Container.Core,
  Spring.Container.Common,
  Spring.Container.LifetimeManager;

type
  /// Simulates the per-scope lifecycle.
  /// The scope is started when the class instance is called and is terminated by middleware in the DMVCFramework

  TSingletonPerScopeLifetimeManager = class(TLifetimeManagerBase)
  private
    fInstances: IDictionary<TThreadID, Func<TValue>>;
  protected
    procedure HandleValueChanged(sender: TObject; const item: Func<TValue>; action: TCollectionChangedAction);
    function CreateHolder(const instance: TValue; refCounting: TRefCounting): Func<TValue>; virtual;
  public
    constructor Create(const model: TComponentModel); override;
    destructor Destroy; override;

    function Resolve(const context: ICreationContext; const model: TComponentModel): TValue; override;
    procedure Release(const instance: TValue); override;
  end;

  TSingletonPerScopeLifetimeManagerRepository = class
  private
    class var FInstances: IDictionary<string, ILifetimeManager>;
  public
    class constructor Create;
    class destructor Destroy;
    class function GetInstance(const AName: string): ILifetimeManager;
    class property Instances: IDictionary<string, ILifetimeManager> read FInstances;
  end;

implementation

uses
  System.Classes;

{ TSingletonPerScopeLifetimeManager }

constructor TSingletonPerScopeLifetimeManager.Create(const model: TComponentModel);
begin
  inherited Create(model);
  fInstances := TCollections.CreateDictionary<TThreadID, Func<TValue>>;
  fInstances.OnValueChanged.Add(HandleValueChanged);
end;

function TSingletonPerScopeLifetimeManager.CreateHolder(const instance: TValue; refCounting: TRefCounting): Func<TValue>;
begin
  Result := TValueHolder.Create(instance, refCounting);
end;

destructor TSingletonPerScopeLifetimeManager.Destroy;
begin
  fInstances := nil;
  inherited Destroy;
end;

procedure TSingletonPerScopeLifetimeManager.HandleValueChanged(sender: TObject; const item: Func<TValue>; action: TCollectionChangedAction);
begin
  if action = caRemoved then
  begin
    DoBeforeDestruction(item);
  end;
end;

procedure TSingletonPerScopeLifetimeManager.Release(const instance: TValue);
var
  threadID: THandle;
  holder: Func<TValue>;
begin
  threadID := TThread.CurrentThread.ThreadID;
  MonitorEnter(Self);
  try
    if fInstances.TryGetValue(threadID, holder) then
    begin
      fInstances.Remove(threadID);
    end;
  finally
    MonitorExit(Self);
  end;
end;

function TSingletonPerScopeLifetimeManager.Resolve(const context: ICreationContext; const model: TComponentModel): TValue;
var
  threadID: THandle;
  instance: TValue;
  holder: Func<TValue>;
begin
  threadID := TThread.CurrentThread.ThreadID;
  MonitorEnter(Self);
  try
    if not fInstances.TryGetValue(threadID, holder) then
    begin
      instance := model.ComponentActivator.CreateInstance(context);
      holder := CreateHolder(instance, model.RefCounting);
      fInstances[threadID] := holder;
      DoAfterConstruction(holder);
    end;
  finally
    MonitorExit(Self);
  end;
  Result := holder;
end;

{ TSingletonPerScopeLifetimeManagerRepository }

class constructor TSingletonPerScopeLifetimeManagerRepository.Create;
begin
  FInstances := TCollections.CreateDictionary<string, ILifetimeManager>;
end;

class destructor TSingletonPerScopeLifetimeManagerRepository.Destroy;
begin
  FInstances := nil;
end;

class function TSingletonPerScopeLifetimeManagerRepository.GetInstance(const AName: string): ILifetimeManager;
var
  LInstance: ILifetimeManager;
begin
  if not FInstances.TryGetValue(AName, LInstance) then
  begin
    LInstance := TSingletonPerScopeLifetimeManager.Create(nil);
    FInstances[AName] := LInstance;
  end;
  Result := LInstance;
end;

end.
