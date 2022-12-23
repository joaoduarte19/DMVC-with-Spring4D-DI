unit UDBContextLifetime;

interface

uses
  Spring,
  Spring.Collections,
  Spring.Container.Core,
  Spring.Container.Common,
  Spring.Container.LifeTimeManager;

type
  TDBContextLifetimeManager = class(TLifetimeManagerBase)
  private
    fInstances: IDictionary<TThreadID, Func<TValue>>;
  protected
    procedure HandleValueChanged(sender: TObject; const item: Func<TValue>; action: TCollectionChangedAction);
    function CreateHolder(const instance: TValue; refCounting: TRefCounting): Func<TValue>; virtual;
  public
    procedure AfterConstruction; override;

    destructor Destroy; override;
    function Resolve(const context: ICreationContext; const model: TComponentModel): TValue; override;
    procedure Release(const instance: TValue); override;
  end;

function DBContextLifetimeManager: ILifetimeManager;

implementation

uses
  System.Classes,
  Spring.DesignPatterns;

var
  gDBContextLifetimeManager: ILifetimeManager = nil;

function DBContextLifetimeManager: ILifetimeManager;
begin
  if not Assigned(gDBContextLifetimeManager) then
  begin
    gDBContextLifetimeManager := TDBContextLifetimeManager.Create(nil);
  end;

  Result := gDBContextLifetimeManager;
end;

{ TDBContextLifetimeManager }

procedure TDBContextLifetimeManager.AfterConstruction;
begin
  inherited;
  fInstances := TCollections.CreateDictionary<TThreadID, Func<TValue>>;
  fInstances.OnValueChanged.Add(HandleValueChanged);
end;

function TDBContextLifetimeManager.CreateHolder(const instance: TValue; refCounting: TRefCounting): Func<TValue>;
begin
  Result := TValueHolder.Create(instance, refCounting);
end;

destructor TDBContextLifetimeManager.Destroy;
begin
  fInstances := nil;
  inherited Destroy;
end;

procedure TDBContextLifetimeManager.HandleValueChanged(sender: TObject; const item: Func<TValue>; action: TCollectionChangedAction);
begin
  if action = caRemoved then
  begin
    DoBeforeDestruction(item);
  end;
end;

procedure TDBContextLifetimeManager.Release(const instance: TValue);
var
  threadID: THandle;
  holder: Func<TValue>;
begin
  threadID := TThread.CurrentThread.ThreadID;
  MonitorEnter(Self);
  try
    if fInstances.TryGetValue(threadID, holder) then
    begin
//      DoBeforeDestruction(holder);
      fInstances.Remove(threadID);
    end;
  finally
    MonitorExit(Self);
  end;

end;

function TDBContextLifetimeManager.Resolve(const context: ICreationContext; const model: TComponentModel): TValue;
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

initialization

finalization

gDBContextLifetimeManager := nil;

end.
