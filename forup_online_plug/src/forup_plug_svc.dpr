program forup_plug_svc;

{$APPTYPE CONSOLE}

{$R *.res}

{$R *.dres}

uses
  System.SysUtils,
  System.DateUtils,
  System.Classes,
  Uconn_module in 'connection\Uconn_module.pas' {conn_module: TDataModule},
  forup_types in 'lib\forup_types.pas',
  entity_base in 'entities\entity_base.pas',
  helpers in 'lib\helpers.pas',
  repository_base in 'repositories\repository_base.pas',
  controller_base in 'controllers\controller_base.pas',
  logger in 'lib\logger.pas',
  orm_attributes in 'lib\orm_attributes.pas',
  jobservice.client_connection in 'entities\jobservice\jobservice.client_connection.pas',
  job_collector in 'job_resource\job_collector.pas',
  job_listner in 'job_resource\job_listner.pas';

var
  fLogger : Tlogger;
begin
  fLogger := Tlogger.Create;

  if THelper.Functions.InitEnv then
    begin
      conn_module := Tconn_module.Create(nil);
      conn_module.ConnectPostgre;
    end
  else
    begin
      fLogger.LogMessage := 'COULD NOT CONFIGURE ENVIORMENT';
      fLogger.LogDate := Now;
      fLogger.LogID := '0001';
      fLogger.writeLog;
    end;

end.
