unit Uconn_module;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.ConsoleUI.Wait, FireDAC.Phys.PGDef, FireDAC.Phys.PG, UniProvider,
  MongoDBUniProvider, DBAccess, Uni, Data.DB, FireDAC.Stan.ExprFuncs;

type
  Tconn_module = class(TDataModule)
    fup_manager: TFDManager;
    fup_postgre: TFDConnection;
    fup_mongo: TUniConnection;
    fup_mongo_provider: TMongoDBUniProvider;
    pg_driver: TFDPhysPgDriverLink;
    fup_sqlite: TFDConnection;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  conn_module: Tconn_module;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

end.
