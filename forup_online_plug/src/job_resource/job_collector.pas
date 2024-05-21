unit job_collector;

interface
uses System.Rtti, System.SysUtils, System.StrUtils, System.DateUtils,
System.JSON, System.Math, System.TypInfo, System.Types, System.Variants,
Uconn_module, orm_attributes, forup_types, logger, helpers, System.Classes,
Data.DB, MongoObjectsUni, System.Generics.Collections;

type
  TjobCollector = class(TObject)
    private
      pgClientID, pgJobID : String;
      EnterpriseID : String;
      LastPrinted : String;
      ConnBD : Tactive_db;
      ClientMongoConnection : TJSONValue;
      procedure updateDtoVenda(id : String);
    public
      function getCurrentBD : Tactive_db;
      constructor Create(aConnBD : Tactive_db; aLastPrinted : String = ''; aEnterpriseID : String = ''); virtual;
      destructor Destroy; override;

      procedure setClientConnection(aClient : String);
      function getCreateJobs : TJSONObject;
      function getJobs : TJSONObject;
      function getCliente(id : String) : TJSONObject;
      function updateJob(id : String) : TJSONObject;
  end;

implementation

{ TjobCollector }

constructor TjobCollector.Create(aConnBD : Tactive_db; aLastPrinted : String = ''; aEnterpriseID : String = '');
begin
  inherited Create;
  Self.ConnBD := aConnBD;
  Self.LastPrinted := aLastPrinted;
  Self.EnterpriseID := aEnterpriseID;
  if not assigned(conn_module) then
    conn_module := Tconn_module.Create(nil);
end;

destructor TjobCollector.Destroy;
begin
  FreeAndNil(Self.ClientMongoConnection);
  if Assigned(conn_module) then
    begin
      case Self.ConnBD of
        adbPostgres: begin
          conn_module.DisconnectPostgre;
        end;
        adbMySQL: ;
        adbMSSQL: ;
        adbFirebird: ;
        adbSQLite: ;
      end;

      FreeAndNil(conn_module);
    end;

  inherited;
end;

function TjobCollector.getCliente(id: String): TJSONObject;
begin
  conn_module.ConnectMongo(Self.ClientMongoConnection);
  Result := TJSONObject(TJSONObject.ParseJSONValue('{}'));
  if conn_module.MongoDBConnected then
    begin
      try
        with conn_module.mongoQryList do
          begin
            Close;
            Sql.Clear;
            Sql.Add('{"find":"DtoPessoa", "filter":{"_id": {"$oid": "'+id+'"}}}');
            Open;

            if not IsEmpty then
              begin
                First;
                Result := TJSONObject(TJSONObject.ParseJSONValue(TMongoDocument(GetObject('DtoPessoa')).Text));
              end;
          end;
      except
        on e : exception do
          begin
            TSingleLogger.Logger.LogMessage := 'COULD NOT FIND THE DtoPessoa ON MONGO DB';
            TSingleLogger.Logger.LogDate := now;
            TSingleLogger.Logger.LogID := '008';
            TSingleLogger.Logger.LogAdditionaInfo := TJSONObject.ParseJSONValue('{"exception_message":"'+
                THelper.Functions.prepare_string_json(e.Message)+'"}');
            TSingleLogger.Logger.writeLog;
            Result := TJSONObject.Create(TJSONPair.Create('error',TSingleLogger.Logger.LogAdditionaInfo));
          end;
      end;
    end;
end;

function TjobCollector.getCreateJobs: TJSONObject;
var
  mDoc : TMongoDocument;
  jobData : TJSONObject;

  DtoVenda : TJSONArray;
  DtoVendaProduto : TJSONArray;
  iVenda: Integer;
begin
  try
    conn_module.ConnectMongo(Self.ClientMongoConnection);
    jobData := TJSONObject.Create(TJSONPair.Create('orders',TJSONValue(TJSONArray.Create)));

    if conn_module.MongoDBConnected then
      begin
        with conn_module.mongoQryList do
          begin
            Close;
            Sql.Clear;
            Sql.Add('{"find":"DtoVenda", "filter":{"Codigo":{$gt:'+Self.LastPrinted+'}, '+
              '"Impresso": false, "OrigemVenda": "PDV", "EmpresaId":"'+Self.EnterpriseID+'"}}');
            Open;

            DtoVenda := TJSONArray.Create;
            if not IsEmpty then
              begin
                First;
                while not Eof do
                  begin
                    mDoc := TMongoDocument(GetObject('DtoVenda'));
                    updateDtoVenda(mDoc.FieldByName['_id'].GetData.AsString.ToLower);
                    DtoVenda.AddElement(TJSONObject.ParseJSONValue(mDoc.Text));
                    Next;
                  end;
              end;

            if DtoVenda.Count > 0 then
              begin
                DtoVendaProduto := TJSONArray.Create;
                for iVenda := 0 to DtoVenda.Count-1 do
                  begin
                    Close;
                    Sql.Clear;
                    Sql.Add('{"find":"DtoVendaProduto", "filter":{"VendaID":"'+
                      DtoVenda.Items[iVenda].GetValue<String>('_id.$oid').ToLower
                    +'"}}');
                    Open;

                    if not IsEmpty then
                      begin
                        First;
                        while not Eof do
                          begin
                            mDoc := TMongoDocument(GetObject('DtoVendaProduto'));
                            DtoVendaProduto.AddElement(TJSONObject.ParseJSONValue(mDoc.Text));
                            Next;
                          end;
                      end;

                    jobData.GetValue<TJSONArray>('orders').AddElement(
                      TJSONObject.ParseJSONValue('{"DtoVenda":'+TJSONValue(DtoVenda.Items[iVenda]).ToJSON+','+
                        '"DtoVendaProduto":'+TJSONValue(DtoVendaProduto).ToJSON+'}')
                    );
                  end;

              end;
          end;
      end;

      if jobData.GetValue<TJSONArray>('orders').Count > 0 then
        begin
          if conn_module.PostgreConnected then
            begin
              with conn_module.pgCmd do
                begin
                  CommandText.Clear;
                  CommandText.Add('INSERT INTO jobservice.jobs_waiting '+
                   '(id, job_id, job_waiting_from, job_data, job_collected_at, job_status) '+
                   'VALUES(gen_random_uuid(), '+QuotedStr(pgJobID)+', now(), '+QuotedStr(jobData.ToJSON)+', null, ''W'');');
                  //CommandText.SaveToFile(THelper.Functions.AppPath+'cmdInsert.sql');
                  Execute;
                end;
            end;
        end;

      with conn_module.pgQryList do
        begin
          Close;
          Sql.Clear;
          Sql.Add('SELECT id, job_id, job_waiting_from, job_data, job_collected_at, job_status');
          Sql.Add('FROM jobservice.jobs_waiting');
          Sql.Add('WHERE job_collected_at is null and job_status = ''W''');
          Open;
        end;

      Result := THelper.Functions.dataset_to_json('jobs_waiting', conn_module.pgQryList);
  except
    on e : Exception do
      begin
        Result := TJSONObject(TJSONObject.ParseJSONValue('{"jobs_waiting":[]}'));

        TSingleLogger.Logger.LogMessage := 'COULD NOT GET JOBS FOR PRINTING';
        TSingleLogger.Logger.LogDate := now;
        TSingleLogger.Logger.LogID := '004';
        TSingleLogger.Logger.LogAdditionaInfo := TJSONObject.ParseJSONValue('{"exception_message":"'+
        THelper.Functions.prepare_string_json(e.Message)+'"}');
        TSingleLogger.Logger.writeLog;
      end;
  end;
end;

function TjobCollector.getCurrentBD: Tactive_db;
begin
  Result := Self.ConnBD;
end;

function TjobCollector.getJobs: TJSONObject;
begin
  if not conn_module.PostgreConnected then
    conn_module.ConnectPostgre;

  try
    with conn_module.pgQryList do
      begin
        Close;
        Sql.Clear;
        Sql.Add('SELECT id, job_id, job_waiting_from, job_data, job_collected_at, job_status');
        Sql.Add('FROM jobservice.jobs_waiting');
        Sql.Add('WHERE job_collected_at is null and job_status = ''W''');
        Open;
      end;

    Result := THelper.Functions.dataset_to_json('jobs_waiting', conn_module.pgQryList);
  except
    on e : exception do
      begin
        TSingleLogger.Logger.LogMessage := 'COULD NOT LOAD JOBS';
        TSingleLogger.Logger.LogDate := now;
        TSingleLogger.Logger.LogID := '009';
        TSingleLogger.Logger.LogAdditionaInfo := TJSONObject.ParseJSONValue('{"exception_message":"'+
            THelper.Functions.prepare_string_json(e.Message)+'"}');
        TSingleLogger.Logger.writeLog;
        Result := TJSONObject.Create(TJSONPair.Create('error',TSingleLogger.Logger.LogAdditionaInfo));
      end;
  end;
end;

procedure TjobCollector.setClientConnection(aClient: String);
var
  jsonStream : TStringStream;
begin
  try
    case Self.ConnBD of
      adbPostgres: begin
        conn_module.ConnectPostgre;
        if conn_module.PostgreConnected then
          begin
            with conn_module.pgQryList do
              begin
                Close;
                Sql.Clear;
                Sql.Add('SELECT cc.*,');
                Sql.Add('(select jc.id');
                Sql.Add(' from jobservice.job_config jc');
                Sql.Add(' where jc.client_id = cc.id');
                Sql.Add(' and jc.job_info ->> ''job_name'' in (''Imprimir Venda MOB'')');
                Sql.Add(') as jobID');
                Sql.Add('FROM jobservice.client_connection cc');
                Sql.Add('WHERE cc.cnpj_cpf = '+QuotedStr(aClient));
                Open;

                if not IsEmpty then
                  begin
                    jsonStream := TStringStream.Create(FieldByName('connection_info').AsString, TEncoding.UTF8);
                    ClientMongoConnection := TJSONObject.ParseJSONValue(jsonStream.DataString);
                    pgClientID := FieldByName('id').AsString;
                    pgJobID := FieldByName('jobID').AsString;
                  end;
              end;
          end;
      end;
      adbMySQL: ;
      adbMSSQL: ;
      adbFirebird: ;
      adbSQLite: ;
    end;
  except
    on e : exception do
      begin
       TSingleLogger.Logger.LogMessage := 'COULD NOT FIND THE DtoPessoa ON MONGO DB';
        TSingleLogger.Logger.LogDate := now;
        TSingleLogger.Logger.LogID := '010';
        TSingleLogger.Logger.LogAdditionaInfo := TJSONObject.ParseJSONValue('{"exception_message":"'+
            THelper.Functions.prepare_string_json(e.Message)+'"}');
        TSingleLogger.Logger.writeLog;
      end;
  end;
end;

procedure TjobCollector.updateDtoVenda(id: String);
begin
  if not conn_module.MongoDBConnected then
    conn_module.ConnectMongo(Self.ClientMongoConnection);
  try
  with conn_module.mongoUpdQry do
    begin
      Close;
      Sql.Clear;
      Sql.Text := '{"update":"DtoVenda", "updates":[{"q":{"_id": {"$oid":"'+id+'"}},"u":{"$set":{"Impresso":true}}}]}';
      ExecSQL;
    end;
  except
    on e : exception do
      begin
        TSingleLogger.Logger.LogMessage := 'COULD NOT UPDATE DtoVenta TO "Impresso"=true';
        TSingleLogger.Logger.LogDate := now;
        TSingleLogger.Logger.LogID := '011';
        TSingleLogger.Logger.LogAdditionaInfo := TJSONObject.ParseJSONValue('{"exception_message":"'+
            THelper.Functions.prepare_string_json(e.Message)+'"}');
        TSingleLogger.Logger.writeLog;
      end;

  end;
end;

function TjobCollector.updateJob(id: String): TJSONObject;
begin
  try
    with conn_module.pgCmd do
      begin
        Close;
        CommandText.Text := 'UPDATE jobservice.jobs_waiting'+
          ' SET job_status=''P'', job_collected_at = now WHERE id='+QuotedStr(id)+';';
        Execute;
      end;
  except
    on e : exception do
      begin
        TSingleLogger.Logger.LogMessage := 'COULD NOT UPDATE JOB STATUS';
        TSingleLogger.Logger.LogDate := now;
        TSingleLogger.Logger.LogID := '012';
        TSingleLogger.Logger.LogAdditionaInfo := TJSONObject.ParseJSONValue('{"exception_message":"'+
            THelper.Functions.prepare_string_json(e.Message)+'"}');
        TSingleLogger.Logger.writeLog;
        Result := TJSONObject.Create(TJSONPair.Create('error',TSingleLogger.Logger.LogAdditionaInfo));
      end;
  end;
end;

end.
