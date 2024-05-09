unit jobservice.client_connection;

interface
uses System.Rtti, System.StrUtils, System.SysUtils, System.IOUtils,
System.Math, System.DateUtils, orm_attributes, entity_base, System.JSON;

type
  {$M+}
  [Ttable('client_connection','jobservice')]
  Tclient_connection = class(Tbase_entity)
  private
    FID: String;
    FCNPJ_CPF: String;
    FAdditional_Info: TJSONValue;
    FConnection_Info: TJSONValue;
  published
    property ID: String read FID write FID;
    property CNPJ_CPF: String read FCNPJ_CPF write FCNPJ_CPF;
    property Additional_Info: TJSONValue read FAdditional_Info write FAdditional_Info;
    property Connection_Info: TJSONValue read FConnection_Info write FConnection_Info;
  end;

implementation

end.
