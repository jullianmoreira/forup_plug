unit repository_base;

interface
uses System.Rtti, System.Classes, System.SysUtils, System.StrUtils, System.WideStrUtils,
System.DateUtils, System.IOUtils, System.Types, System.TypInfo, System.JSON,
Generics.Collections, forup_types, entity_base;

type

  TIbase_repository = interface
    ['{D131D9E0-0E6E-472B-8EBA-ECFAFA37C1B2}']

    function list(criteria : Tcriteria) : TJSONObject;
    function insert(entity : Tbase_entity) : TJSONObject;
    function update(entity : Tbase_entity) : TJSONObject;
    function delete(criteria : Tcriteria) : TJSONObject;
  end;


  Tbase_repository = class(TInterfacedObject, TIbase_repository)
    private
      FActiveDB : Tactive_db;
    public
      function list(criteria : Tcriteria) : TJSONObject; virtual;
      function insert(entity : Tbase_entity) : TJSONObject; virtual;
      function update(entity : Tbase_entity) : TJSONObject; virtual;
      function delete(criteria : Tcriteria) : TJSONObject; virtual;

      constructor Create(aDataBase : Tactive_db);
  end;

implementation
uses Uconn_module, helpers;
{ Tbase_entity }

constructor Tbase_repository.Create(aDataBase: Tactive_db);
begin
  inherited Create;
  Self.FActiveDB := aDataBase;
end;

function Tbase_repository.delete(criteria : Tcriteria) : TJSONObject;
begin
  Result := TJSONObject.Create;
end;

function Tbase_repository.insert(entity : Tbase_entity) : TJSONObject;
begin
  Result := TJSONObject.Create;
end;

function Tbase_repository.list(criteria: Tcriteria): TJSONObject;
begin
  Result := TJSONObject.Create;
end;

function Tbase_repository.update(entity : Tbase_entity) : TJSONObject;
begin
  Result := TJSONObject.Create;
end;

end.
