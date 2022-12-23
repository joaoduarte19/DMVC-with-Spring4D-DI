unit UDBContext.Interfaces;

interface

uses
  Data.DB;

type
  IDBContext = interface
    ['{A1A1696B-0E91-455C-B594-6A24E638FDA0}']
    function OpenQuery(const ASQL: string): TDataSet;
    function ExecSQL(const ASQL: string): Integer;
  end;

implementation

end.
