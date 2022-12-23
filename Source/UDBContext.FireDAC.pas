unit UDBContext.FireDAC;

interface

uses
  UDBContext.Interfaces,
  System.Classes,
  Data.DB,
  FireDAC.Comp.Client;

type
  TDBContextFireDAC = class(TInterfacedObject, IDBContext)
  private
    FGuid: TGuid;
    FConnection: TFDConnection;
  public
    constructor Create;
    destructor Destroy; override;
    function OpenQuery(const ASQL: string): TDataSet;
    function ExecSQL(const ASQL: string): Integer;
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.Logger,
  Spring.Container,
  UDBContextLifetime;

{ TDBContextFireDAC }

constructor TDBContextFireDAC.Create;
begin
  inherited Create;

  FGuid := TGuid.NewGuid;
  FConnection := TFDConnection.Create(nil);

  Log.Info('%s - TDBContextFireDAC.Create', [FGuid.ToString], 'life_cicle');
end;

destructor TDBContextFireDAC.Destroy;
begin
  FreeAndNil(FConnection);

  Log.Info('%s - TDBContextFireDAC.Destroy', [FGuid.ToString], 'life_cicle');

  inherited;
end;

function TDBContextFireDAC.ExecSQL(const ASQL: string): Integer;
begin
  Result := FConnection.ExecSQL(ASQL);
end;

function TDBContextFireDAC.OpenQuery(const ASQL: string): TDataSet;
var
  LQuery: TFDQuery;
begin
  LQuery := TFDQuery.Create(nil);
  try
    LQuery.Open(ASQL);
    Result := LQuery;
  except
    FreeAndNil(LQuery);
    raise;
  end;
end;

initialization

GlobalContainer.RegisterType<IDBContext, TDBContextFireDAC>.AsCustom(DBContextLifetimeManager);

end.
