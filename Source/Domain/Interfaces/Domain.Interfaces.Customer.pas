unit Domain.Interfaces.Customer;

interface

uses
  System.Generics.Collections,
  Domain.Entities.Customer;

type
  ICustomerService = interface
    ['{A4968213-C7F2-4B17-8319-43BD02A68833}']
    function GetAllCustomers: TObjectList<TCustomer>;
    function GetCustomer(const AId: Integer): TCustomer;
    function InsertCustomer(const ACustomer: TCustomer): Boolean;
    function UpdateCustomer(const ACustomer: TCustomer): Boolean;
    function DeleteCustomer(const AId: Integer): Boolean;
  end;

  ICustomerRepository = interface
    ['{3E8D1629-B5B9-44C9-91B7-00E376145E9A}']
    function GetAllCustomers: TObjectList<TCustomer>;
    function GetCustomer(const AId: Integer): TCustomer;
    function GetCountCustomers: Integer;
    function InsertCustomer(const ACustomer: TCustomer): Boolean;
    function UpdateCustomer(const ACustomer: TCustomer): Boolean;
    function DeleteCustomer(const AId: Integer): Boolean;
  end;

implementation

end.
