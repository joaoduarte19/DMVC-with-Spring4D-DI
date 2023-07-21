unit Application.Services.Customer;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Domain.Interfaces.Customer,
  Domain.Entities.Customer;

type
  TCustomerService = class(TInterfacedObject, ICustomerService)
  private
    FGuid: TGuid;
    FCustomerRepository: ICustomerRepository;
  public
    constructor Create(const ACustomerRepository: ICustomerRepository);
    destructor Destroy; override;

    function GetAllCustomers: TObjectList<TCustomer>;
    function GetCustomer(const AId: Integer): TCustomer;
    function InsertCustomer(const ACustomer: TCustomer): Boolean;
    function UpdateCustomer(const ACustomer: TCustomer): Boolean;
    function DeleteCustomer(const AId: Integer): Boolean;
  end;

implementation

uses
  MVCFramework.Logger;

{ TCustomerService }

constructor TCustomerService.Create(const ACustomerRepository: ICustomerRepository);
begin
  FGuid := TGuid.NewGuid;
  FCustomerRepository := ACustomerRepository;
  Log.Info('%s - TCustomerService.Create', [FGuid.ToString], 'life_cicle');
end;

function TCustomerService.DeleteCustomer(const AId: Integer): Boolean;
begin
  Result := FCustomerRepository.DeleteCustomer(Aid);
end;

destructor TCustomerService.Destroy;
begin
  Log.Info('%s - TCustomerService.Destroy', [FGuid.ToString], 'life_cicle');
  inherited;
end;

function TCustomerService.GetAllCustomers: TObjectList<TCustomer>;
begin
  Result := FCustomerRepository.GetAllCustomers;
end;

function TCustomerService.GetCustomer(const AId: Integer): TCustomer;
begin
  Result := FCustomerRepository.GetCustomer(AId);
end;

function TCustomerService.InsertCustomer(const ACustomer: TCustomer): Boolean;
begin
  // Business logic
  ACustomer.Id := FCustomerRepository.GetCountCustomers + 1;

  Result := FCustomerRepository.InsertCustomer(ACustomer);
end;

function TCustomerService.UpdateCustomer(const ACustomer: TCustomer): Boolean;
begin
  Result := FCustomerRepository.UpdateCustomer(ACustomer);
end;

end.
