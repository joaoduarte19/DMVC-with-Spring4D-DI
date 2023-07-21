unit Domain.Entities.Customer;

interface

type
  TCustomer = class
  private
    FId: Integer;
    FName: string;
    FDateOfBirth: TDate;
    FCpf: string;
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property DateOfBirth: TDate read FDateOfBirth write FDateOfBirth;
    property Cpf: string read FCpf write FCpf;
  end;

implementation

end.
