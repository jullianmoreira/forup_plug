program forup_plug_svc;

{$IFNDEF WINDOWS}
  {$DEFINE HORSE_DAEMON}
{$ENDIF}

{$APPTYPE CONSOLE}

{$R *.res}

{$R *.dres}

uses
  System.SysUtils,
  System.DateUtils,
  System.Classes,
  System.JSON,
  Horse,
  Horse.Jhonson,
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
  collector : TjobCollector;
begin
  fLogger := Tlogger.Create;

  if THelper.Functions.InitEnv then
    begin
      THorse.Use(Jhonson('utf-8'));
      THorse.Get('jobs/:lastone/:emprid/:cnpj',
        procedure(Req: THorseRequest; Res: THorseResponse)
          var
            jobs : TJSONObject;
          begin
            //'64f2705f6b00c1f593efd30f'
            //'19503009000143'
            //collector.getCreateJobs;
            collector := TjobCollector.Create(adbPostgres, Req.Params['lastone'], Req.Params['emprid']);
            collector.setClientConnection(Req.Params['cnpj']);

            jobs := collector.getJobs;
            if jobs.GetValue<TJSONArray>('jobs_waiting').Count = 0 then
              jobs := collector.getCreateJobs;

            if jobs.GetValue<TJSONArray>('jobs_waiting').Count > 0 then
              Res.Send<TJSONObject>(jobs)
            else
              begin
                Res.Send<TJSONObject>(
                  TJSONObject.Create(
                    TJSONPair.Create(
                      'jobs_waiting',
                      TJSONArray.Create
                    )
                  )
                );
              end;

            collector.Destroy;
            //FreeAndNil(jobs);
          end
      );
      {$IFDEF HORSE_DAEMON}
        THorse.Port := 9090;
        THorse.Listen;
      {$ELSE}
        THorse.Listen(9090);
      {$ENDIF}
    end
  else
    begin
      fLogger.LogMessage := 'COULD NOT CONFIGURE ENVIORMENT';
      fLogger.LogDate := Now;
      fLogger.LogID := '0001';
      fLogger.writeLog;
    end;

end.
