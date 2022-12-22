unit UCustomer.Repository;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  UCustomer.Interfaces,
  UCustomer.Entity,
  MVCFramework.Logger,
  Spring.Collections;

type
  TCustomerRepository = class(TInterfacedObject, ICustomerRepository)
  private
    class var FCustomers: IList<TCustomer>;
    class function CloneCustomer(const ACustomer: TCustomer): TCustomer;
    class function ConvertIListToTObjectList(const AList: IList<TCustomer>): TObjectList<TCustomer>;
  private
    FGuid: TGuid;
  public
    class constructor Create;

    constructor Create;
    destructor Destroy; override;

    function GetAllCustomers: TObjectList<TCustomer>;
    function GetCustomer(const AId: Integer): TCustomer;
    function GetCountCustomers: Integer;
    function InsertCustomer(const ACustomer: TCustomer): Boolean;
    function UpdateCustomer(const ACustomer: TCustomer): Boolean;
    function DeleteCustomer(const AId: Integer): Boolean;
  end;

implementation

uses
  Spring.Container;

{ TCustomerRepository }

class function TCustomerRepository.CloneCustomer(const ACustomer: TCustomer): TCustomer;
begin
  Result := TCustomer.Create;
  Result.Id := ACustomer.Id;
  Result.Name := ACustomer.Name;
  Result.DateOfBirth := ACustomer.DateOfBirth;
  Result.Cpf := ACustomer.Cpf;
end;

class function TCustomerRepository.ConvertIListToTObjectList(const AList: IList<TCustomer>): TObjectList<TCustomer>;
var
  LObjectList: TObjectList<TCustomer>;
begin
  LObjectList := TObjectList<TCustomer>.Create;
  AList.ForEach(
    procedure(const AItem: TCustomer)
    begin
      LObjectList.Add(CloneCustomer(AItem));
    end
    );
  Result := LObjectList;
end;

constructor TCustomerRepository.Create;
begin
  FGuid := TGuid.NewGuid;
  Log.Info('%s - TCustomerRepository.Create', [FGuid.ToString], 'life_cicle');
end;

class constructor TCustomerRepository.Create;
begin
  FCustomers := TCollections.CreateObjectList<TCustomer>;
end;

function TCustomerRepository.DeleteCustomer(const AId: Integer): Boolean;
begin
  Result := FCustomers.RemoveAll(
    function(const AItem: TCustomer): Boolean
    begin
      Result := AItem.Id = AId;
    end
    ) > 0;
end;

destructor TCustomerRepository.Destroy;
begin
  Log.Info('%s - TCustomerRepository.Destroy', [FGuid.ToString], 'life_cicle');

  inherited;
end;

function TCustomerRepository.GetAllCustomers: TObjectList<TCustomer>;
begin
  Result := ConvertIListToTObjectList(FCustomers);
end;

function TCustomerRepository.GetCountCustomers: Integer;
begin
  Result := FCustomers.Count;
end;

function TCustomerRepository.GetCustomer(const AId: Integer): TCustomer;
var
  LCustomer: TCustomer;
begin
  Result := nil;
  if FCustomers.TryGetSingle(LCustomer,
    function(const AItem: TCustomer): Boolean
    begin
      Result := AItem.Id = AId;
    end) then
  begin
    Result := CloneCustomer(LCustomer);
  end;
end;

function TCustomerRepository.InsertCustomer(const ACustomer: TCustomer): Boolean;
begin
  FCustomers.Add(CloneCustomer(ACustomer));
  Result := True;
end;

function TCustomerRepository.UpdateCustomer(const ACustomer: TCustomer): Boolean;
var
  LCustomer: TCustomer;
begin
  Result := False;
  LCustomer := GetCustomer(ACustomer.Id);
  if FCustomers.TryGetSingle(LCustomer,
    function(const AItem: TCustomer): Boolean
    begin
      Result := AItem.Id = ACustomer.Id;
    end) then
  begin
    LCustomer.Name := ACustomer.Name;
    LCustomer.DateOfBirth := ACustomer.DateOfBirth;
    LCustomer.Cpf := ACustomer.Cpf;
    Result := True;
  end;
end;

initialization

GlobalContainer.RegisterType<TCustomerRepository>.Implements<ICustomerRepository>;

end.
