unit Uconn_module;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.ConsoleUI.Wait, FireDAC.Phys.PGDef, FireDAC.Phys.PG, UniProvider,
  MongoDBUniProvider, DBAccess, Uni, Data.DB, FireDAC.Stan.ExprFuncs,
  forup_types, FireDAC.Comp.UI, FireDAC.Phys.MySQLDef, FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.ODBCBase, FireDAC.Phys.MSSQL, FireDAC.Phys.MySQL, System.JSON,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet, MemDS;

const
  PG_DRV = 'drv'+PathDelim+'pgsql'+PathDelim+'libpq.dll';
  MYSQL_DRV = 'drv'+PathDelim+'mysql'+PathDelim+'libmysql.dll';
  MSSQL_DRV = 'drv'+PathDelim+'mssql'+PathDelim+'msodbcsql13.dll';

type
  Tconn_module = class(TDataModule)
    fup_manager: TFDManager;
    fup_postgre: TFDConnection;
    fup_mongo: TUniConnection;
    fup_mongo_provider: TMongoDBUniProvider;
    pg_driver: TFDPhysPgDriverLink;
    fup_sqlite: TFDConnection;
    fdWait: TFDGUIxWaitCursor;
    mysql_driver: TFDPhysMySQLDriverLink;
    mssql_driver: TFDPhysMSSQLDriverLink;
    pgQryList: TFDQuery;
    mongoQryList: TUniQuery;
    pgCmd: TFDCommand;
    mongoUpdQry: TUniQuery;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    procedure ConfigureManager;
  public
    { Public declarations }
    PostgreConnected : Boolean;
    MongoDBConnected : Boolean;
    procedure ConnectPostgre;
    procedure DisconnectPostgre;
    procedure ConnectMongo(aConnection : TJSONValue);
    procedure DisconnectMongo;
  end;

var
  conn_module: Tconn_module;

implementation
uses helpers, logger;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

{ Tconn_module }

procedure Tconn_module.ConfigureManager;
begin
  {$IFDEF MSWINDOWS}
    pg_driver.VendorLib := (THelper.Functions.AppPath+CONN_CFG_DIR+PG_DRV);
    pg_driver.VendorLib := StringReplace(pg_driver.VendorLib, PathDelim+PathDelim, PathDelim, [rfReplaceAll]);
  {$ENDIF}
  if not fup_manager.ConnectionDefFileLoaded then
    begin
      fup_manager.Active := false;
      fup_manager.ConnectionDefFileName := THelper.Functions.AppPath+CONN_CFG_DIR+CONN_CFG_FILE;
      fup_manager.LoadConnectionDefFile;
      fup_manager.Active := true;
    end;
end;

procedure Tconn_module.ConnectMongo(aConnection: TJSONValue);
var
  log : Tlogger;
begin
  if aConnection <> nil then
    begin
      with fup_mongo do
        begin
          {$IFDEF WINDOWS}
            {$IFDEF WIN32}
              SpecificOptions.Values['MongoDB.BSONLibrary'] := THelper.Functions.mongoLib32+'libbson-1.0.dll';
              SpecificOptions.Values['MongoDB.ClientLibrary'] := THelper.Functions.mongoLib32+'libmongoc-1.0.dll';
            {$ELSE}
              SpecificOptions.Values['MongoDB.BSONLibrary'] := THelper.Functions.mongoLib64+'libbson-1.0.dll';
              SpecificOptions.Values['MongoDB.ClientLibrary'] := THelper.Functions.mongoLib64+'libmongoc-1.0.dll';
            {$ENDIF}
          {$ENDIF}

          SpecificOptions.Values['MongoDB.ConnectionFormat'] := 'cfStandard';

          Database := aConnection.GetValue<String>('database');
          Port := 27017;
          Server := aConnection.GetValue<String>('srv');
          Username := aConnection.GetValue<String>('user');
          Password := aConnection.GetValue<String>('senha');
          try
            Connect;
            MongoDBConnected := Connected;
          except
            on e : Exception do
              begin
                log := Tlogger.Create;
                log.LogMessage := 'COULD NOT CONNECT ON MONGODB';
                log.LogDate := now;
                log.LogID := '003';
                log.LogAdditionaInfo := TJSONObject.ParseJSONValue('{"exception_message":"'+
                    THelper.Functions.prepare_string_json(e.Message)+'"}');
                log.writeLog;
                log.Destroy;
              end;
          end;
        end;
    end;
end;

procedure Tconn_module.ConnectPostgre;
var
  logger : Tlogger;
begin
  try
    with fup_postgre do
      begin
        Close;
        LoginPrompt := false;
        ConnectionDefName := 'POSTGRE';
        Open;
        PostgreConnected := Connected;
      end;
  except
    on e : exception do
      begin
        logger := Tlogger.Create;
        logger.LogID := '0002';
        logger.LogMessage := 'COULD NOT CONNECT ON POSTGRE DATABASE';
        logger.LogDate := Now;
        logger.LogAdditionaInfo := TJSONObject.ParseJSONValue('{"exception_message":"'+
          THelper.Functions.prepare_string_json(e.Message)+'"}');
        logger.writeLog;
        logger.Destroy;
      end;
  end;
end;

procedure Tconn_module.DataModuleCreate(Sender: TObject);
begin
  PostgreConnected := False;
  ConfigureManager;
end;

procedure Tconn_module.DisconnectMongo;
begin

end;

procedure Tconn_module.DisconnectPostgre;
begin
  with fup_postgre do
    begin
      if InTransaction then
        Commit;

      Close;
    end;
end;

end.
