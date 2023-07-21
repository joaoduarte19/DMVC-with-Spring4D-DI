unit Application.Controllers.Customer;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  Domain.Interfaces.Customer,
  Spring.Container.Common;

type

  [MVCPath('/api')]
  TCustomerController = class(TMVCController)
  private
    FGuid: TGuid;
    [Inject]
    FCustomerService: ICustomerService;
  public
    constructor Create; override;
    destructor Destroy; override;

    // Sample CRUD Actions for a "Customer" entity
    [MVCPath('/customers')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomers;

    [MVCPath('/customers/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomer(id: Integer);

    [MVCPath('/customers')]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateCustomer;

    [MVCPath('/customers/($id)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdateCustomer(id: Integer);

    [MVCPath('/customers/($id)')]
    [MVCHTTPMethod([httpDELETE])]
    procedure DeleteCustomer(id: Integer);

  end;

implementation

uses
  System.SysUtils,
  MVCFramework.Logger,
  Domain.Entities.Customer;

// Sample CRUD Actions for a "Customer" entity
procedure TCustomerController.GetCustomers;
begin
  Render<TCustomer>(FCustomerService.GetAllCustomers);
end;

procedure TCustomerController.GetCustomer(id: Integer);
begin
  Render(FCustomerService.GetCustomer(id));
end;

constructor TCustomerController.Create;
begin
  inherited Create;
  FGuid := TGuid.NewGuid;
// FCustomerService := ACustomerService;
  Log.Info('%s - TCustomerController.Create', [FGuid.ToString], 'life_cicle');
end;

procedure TCustomerController.CreateCustomer;
var
  LCustomer: TCustomer;
begin
  LCustomer := Context.Request.BodyAs<TCustomer>;
  try
    FCustomerService.InsertCustomer(LCustomer);
    ResponseStatus(201, 'Created');
  finally
    LCustomer.Free;
  end;
end;

procedure TCustomerController.UpdateCustomer(id: Integer);
var
  LCustomer: TCustomer;
begin
  LCustomer := Context.Request.BodyAs<TCustomer>;
  try
    FCustomerService.UpdateCustomer(LCustomer);
    ResponseStatus(200, 'Updated');
  finally
    LCustomer.Free;
  end;
end;

procedure TCustomerController.DeleteCustomer(id: Integer);
begin
  FCustomerService.DeleteCustomer(id);
  ResponseStatus(200, 'Ok');
end;

destructor TCustomerController.Destroy;
begin
  Log.Info('%s - TCustomerController.Destroy', [FGuid.ToString], 'life_cicle');
  inherited;
end;

end.
