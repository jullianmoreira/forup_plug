unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus, Vcl.Buttons, System.StrUtils,
  Vcl.Imaging.pngimage, Vcl.StdCtrls, System.ImageList, Vcl.ImgList, System.UITypes,
  Vcl.WinXCtrls, Vcl.Samples.Spin, System.IniFiles, System.IOUtils, Job_Executor,
  Vcl.Printers, Winapi.WinSvc, Winapi.WinInet, frxClass, frxDBSet, System.JSON,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat, Datasnap.DBClient, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, System.DateUtils,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet, System.Generics.Collections, MemDS, DBAccess, Uni,
  UniProvider, MongoDBUniProvider, MongoObjectsUni, System.Zip;

const
  FMT_LOGLINE = '%s -> %s -> %s';
  CFG_FILE = 'cfg.ini';
  CLIENT_PATH = 'data';
  REPORT_PATH = 'reports';
  FILIAL_FILE = 'filial.json';

  INI_SECTION_LOCAL_CONFIG = 'LOCAL CONFIG';
  INI_OPTION_HOST = 'API_HOST';
  INI_OPTION_CLIENT_CNPJ = 'CLIENT_CNPJ';
  INI_OPTION_CLIENT_EMPR_ID = 'CLIENT_EMPR_ID';
  INI_OPTION_CLIENT_LAST_PRINT = 'CLIENT_LAST_PRINT';
  INI_OPTION_DEFAULT_PRINTER = 'DEFAULT_PRINTER';
  INI_OPTION_DEFAULT_MODEL = 'DEFAULT_MODEL';
  INI_OPTION_DEFAULT_TIMER = 'DEFAULT_TIMER';

  GREEN_STATUS = 'GREENSTS';
  RED_STATUS = 'REDSTS';


type
  {$M+}
  TplugReport = class(TObject)
  private
    FFileName: String;
    FFullPath: String;
    FExt: String;
  public
    constructor Create(aFullPath : String);
  published
    property FileName: String read FFileName write FFileName;
    property Ext: String read FExt write FExt;
    property FullPath: String read FFullPath write FFullPath;
  end;
  TSvcStatus = (ssLocal, ssOnline, ssSpooler);
  TfrmMain = class(TForm)
    trayIcon: TTrayIcon;
    popTray: TPopupMenu;
    popShowForm: TMenuItem;
    popCloseSystem: TMenuItem;
    shpRight: TShape;
    shpLeft: TShape;
    shpTop: TShape;
    shpBottom: TShape;
    pnMainContent: TPanel;
    pnFormCaption: TPanel;
    btCloseForm: TSpeedButton;
    pnDivButtons: TPanel;
    btMinimize: TSpeedButton;
    imgIco: TImage;
    lblFormCaption: TLabel;
    gbStatus: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    imgLocalServiceStatus: TImage;
    imgOnlineServiceStatus: TImage;
    imgSpoolerStatus: TImage;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    cbTicketPrinter: TComboBox;
    cbPrintModel: TComboBox;
    spPrintInterval: TSpinEdit;
    switchLocalService: TToggleSwitch;
    GroupBox2: TGroupBox;
    memLog: TMemo;
    Label8: TLabel;
    service: TTimer;
    reports: TfrxReport;
    repData: TfrxDBDataset;
    conLocal: TFDConnection;
    btUpdateConfig: TSpeedButton;
    apiClient: TRESTClient;
    apiRequest: TRESTRequest;
    apiResponse: TRESTResponse;
    btReimprimir: TSpeedButton;
    Label9: TLabel;
    edtPediCodigo: TEdit;
    localQry: TFDQuery;
    cbPrintTime: TComboBox;
    mongoDB: TUniConnection;
    mongoQry: TUniQuery;
    mongoCMD: TUniQuery;
    provider: TMongoDBUniProvider;
    procedure popShowFormClick(Sender: TObject);
    procedure trayIconDblClick(Sender: TObject);
    procedure btCloseFormClick(Sender: TObject);
    procedure popCloseSystemClick(Sender: TObject);
    procedure btMinimizeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure switchLocalServiceClick(Sender: TObject);
    procedure serviceTimer(Sender: TObject);
    procedure btUpdateConfigClick(Sender: TObject);
    procedure btReimprimirClick(Sender: TObject);
  private
    { Private declarations }
    checkThread : TServiceChecker;
    thisFilial : TJSONObject;
    monoConexao : TJSONValue;
    EmprID, EmprCNPJ : String;

    procedure updateDtoVenda(id : String);

    procedure mongoEnv;
    function mongoLib32 : String;
    function mongoLib64 : String;

    procedure ConectarMongo;
    procedure DesconectarMongo;

    function concatStr(aData : TArray<String>) : String;
    function coalesce(aData : TArray<String>) : String;
    function fmtJsonDate(aDate : String; withHour : Boolean = true) : String;

    function getFmtTime : String;
    function getURL_HeartBeat : String;
    procedure setStatusImagem(service : TSvcStatus; image : String);

    function getAppPath : String;
    function getCFGFile : TIniFile;

    procedure CloseMe;
    procedure ShowMe;
    procedure HideMe;

    procedure CheckEnviorment;
    procedure LoadPrinters;
    procedure LoadReports;
    procedure SetFilial;
    procedure LocalCache;

    function getClientData(id, cnpj : String) : TJSONObject;
    function getJobsFromServer : TJSONObject;
    function getOrderFromServer(cod : String) : TJSONObject;
    function getClientData_(id, cnpj : String) : TJSONObject;
    function getJobsFromServer_ : TJSONObject;
    function getOrderFromServer_(cod : String) : TJSONObject;
  public
    { Public declarations }
    cfgFile : TIniFile;

    function getHost : String;

    procedure LimparLog;
    procedure getJobs;
    procedure updateJobStatus(id : String);
    procedure loadToCache(jobs : TJSONObject);
    procedure saveOrderToCache(id : String; order, itens : String);
    procedure printOrder(order : TJSONObject);

    function getOrderPayment(payments : TJSONArray) : String;
    procedure updateLastOne;

    function getCDSItens : TClientDataSet;

    procedure CheckSpooler;
    procedure CallBackSpooler(isTrue : Boolean);
    procedure CheckOnlineService;
    procedure CallBackOnlineService(isTrue : Boolean);
    procedure CheckLocarService;
    procedure CallBackLocalService(isTrue : Boolean);
  end;

var
  frmMain: TfrmMain;
  StopService : Boolean;
  DoingAction : Boolean;


implementation

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.btCloseFormClick(Sender: TObject);
begin
  CloseMe;
end;

procedure TfrmMain.btMinimizeClick(Sender: TObject);
begin
  HideMe;
end;

procedure TfrmMain.btReimprimirClick(Sender: TObject);
var
  order : TJSONObject;
begin
  try
    btReimprimir.Enabled := false;
    if edtPediCodigo.Text <> EmptyStr then
      begin
        DoingAction := true;
        case cbPrintTime.ItemIndex of
          0 : begin
              order := getOrderFromServer_(edtPediCodigo.Text);

              if order.GetValue<TJSONArray>('orders').Count > 0 then
                begin
                  printOrder(TJSONObject(order.GetValue<TJSONArray>('orders').Items[0]));
                end
              else
                begin
                  memLog.Lines.Add(
                    Format(
                      FMT_LOGLINE,
                      [getFmtTime,
                      'VENDA NÃO LOCALIZADA',
                      'VENDA: "'+edtPediCodigo.Text+'"'
                      ]
                    )
                  )
                end;
          end;
          1 : begin
              with localQry do
                begin
                  Close;
                  Sql.Clear;
                  Sql.Add('SELECT * FROM orders WHERE order_id = '+edtPediCodigo.Text);
                  Open;
                  if not IsEmpty then
                    begin
                      order := TJSONObject.Create
                        .AddPair(
                            TJSONPair.Create('DtoVenda',
                                             TJSONObject.ParseJSONValue(FieldByName('order_json').AsString)
                            )
                        )
                        .AddPair(
                            TJSONPair.Create('DtoVendaProduto',
                                             TJSONArray(TJSONObject.ParseJSONValue(FieldByName('order_itens_json').AsString))
                            )
                        );

                      printOrder(order);
                    end;
                end;
          end;
        end;
        DoingAction := false;
      end;
  finally
        btReimprimir.Enabled := true;
  end;
end;

procedure TfrmMain.btUpdateConfigClick(Sender: TObject);
begin
  try
    btUpdateConfig.Enabled := false;
    cfgFile.WriteString(INI_SECTION_LOCAL_CONFIG, INI_OPTION_DEFAULT_PRINTER, cbTicketPrinter.Text);
    cfgFile.WriteString(INI_SECTION_LOCAL_CONFIG, INI_OPTION_DEFAULT_MODEL, cbPrintModel.Text);
    cfgFile.WriteInteger(INI_SECTION_LOCAL_CONFIG, INI_OPTION_DEFAULT_TIMER, spPrintInterval.Value);
    MessageDlg('Configuração Salva!',TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
  finally
    btUpdateConfig.Enabled := true;
  end;
end;

procedure TfrmMain.CallBackLocalService(isTrue: Boolean);
begin
  setStatusImagem(ssLocal, IfThen(isTrue, GREEN_STATUS, RED_STATUS));
end;

procedure TfrmMain.CallBackOnlineService(isTrue: Boolean);
begin
  setStatusImagem(ssOnline, IfThen(isTrue, GREEN_STATUS, RED_STATUS));
end;

procedure TfrmMain.CallBackSpooler(isTrue: Boolean);
begin
  setStatusImagem(ssSpooler, IfThen(isTrue, GREEN_STATUS, RED_STATUS));
end;

procedure TfrmMain.CheckEnviorment;
var
  timerVal : Integer;
  midas : TResourceStream;

begin
  cfgFile := getCFGFile;

  if not TDirectory.Exists(getAppPath+CLIENT_PATH) then
    TDirectory.CreateDirectory(getAppPath+CLIENT_PATH);
  if not TDirectory.Exists(getAppPath+REPORT_PATH) then
    TDirectory.CreateDirectory(getAppPath+REPORT_PATH);

  mongoEnv;

  {$IFDEF WIN32}
    midas := TResourceStream.Create(HInstance, 'MIDAS32', RT_RCDATA);
  {$ELSE}
    midas := TResourceStream.Create(HInstance, 'MIDAS64', RT_RCDATA);
  {$ENDIF}

  if not TFile.Exists(getAppPath+'midas.dll') then
    midas.SaveToFile(getAppPath+'midas.dll');

  LoadPrinters;
  LoadReports;

  SetFilial;

  ConectarMongo;
  CallBackOnlineService(OnlineServiceActive);

  LocalCache;

  timerVal := cfgFile.ReadInteger(INI_SECTION_LOCAL_CONFIG, INI_OPTION_DEFAULT_TIMER, 30);
  service.Interval := timerVal*1000;
  spPrintInterval.Value := timerVal;
  service.Enabled := true;

  checkThread := TServiceChecker.Create(
    @LocalServiceActive, nil, @SpoolerActive,
    CallBackLocalService, nil, CallBackSpooler
  );
  checkThread.OnlineChecker := nil;
  checkThread.SpoolerChecker := CheckSpooler;
  checkThread.LocalChecker := CheckLocarService;

  checkThread.Start;
end;

procedure TfrmMain.CheckLocarService;
begin
  LimparLog;
  LocalServiceActive := false;
  LocalServiceActive := service.Enabled;
end;

procedure TfrmMain.CheckOnlineService;
var
  hInternet : Pointer;
  hConnect: Pointer;
  dwAccessType: DWORD;
  dwFlags: DWORD;
  URL : String;
begin
  OnlineServiceActive := false;
  URL := getURL_HeartBeat;
  try
    LimparLog;
    if URL.IsEmpty then
      begin
        memLog.Lines.Add(Format(
          FMT_LOGLINE, [
            getFmtTime, 'CHECAGEM DO HOST', 'Sem configuração de Host do Serviço Online'
          ]
        ));
      end
    else
      begin
        // Verifica se há conexão com a internet local
        dwAccessType := INTERNET_OPEN_TYPE_DIRECT;
        hInternet := InternetOpen(nil, dwAccessType, nil, nil, 0);
        if hInternet <> nil then
        begin
          // Abre uma conexão com o URL especificado
          dwFlags := 0;
          hConnect := InternetOpenUrl(hInternet, PChar(URL), nil, 0, dwFlags, 0);
          if hConnect <> nil then
          begin
            setStatusImagem(ssOnline, GREEN_STATUS);
            OnlineServiceActive := true;
            InternetCloseHandle(hConnect);
          end
          else
            begin
              setStatusImagem(ssOnline, RED_STATUS); // O serviço não está respondendo
              InternetCloseHandle(hInternet);
              memLog.Lines.Add(Format(
                FMT_LOGLINE, [
                  getFmtTime, 'CHECAGEM DO HOST', 'Sem resposta de: "'+URL+'"'
                ]
              ));
            end;
        end
        else
          memLog.Lines.Add(Format(
          FMT_LOGLINE, [
            getFmtTime, 'CHECAGEM DO HOST', 'Sem conexão com a internet'
          ]
        ));
      end;
  except
    on e : exception do
      begin
        memLog.Lines.Add(Format(
          FMT_LOGLINE, [
            getFmtTime, 'CHECAGEM DO HOST', 'Erro: '+e.Message]));
      end;
  end;
end;

procedure TfrmMain.CheckSpooler;
var
  ServiceManager: SC_Handle;
  ServiceHandle: SC_Handle;
  ServiceStatus: TServiceStatus;
begin
  SpoolerActive := false;
  ServiceManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  try
    LimparLog;
    if ServiceManager <> 0 then
    begin
      // Abre o serviço de spooler
      ServiceHandle := OpenService(ServiceManager, 'Spooler', SERVICE_QUERY_STATUS);
      if ServiceHandle <> 0 then
      begin
        // Verifica o status do serviço
        if QueryServiceStatus(ServiceHandle, ServiceStatus) then
        begin
          case ServiceStatus.dwCurrentState of
            {SERVICE_STOPPED, SERVICE_PAUSED, SERVICE_START_PENDING,
            SERVICE_STOP_PENDING, SERVICE_CONTINUE_PENDING, SERVICE_PAUSE_PENDING:}
            SERVICE_RUNNING: begin
              SpoolerActive := true;
            end;
          end;
        end
        else
          memLog.Lines.Add(Format(FMT_LOGLINE,[
            getFmtTime, 'CHECAGEM DE SPOOLER', 'Erro ao consultar o status do Spooler de Impressão'])
          );
        // Fecha o handle do serviço
        CloseServiceHandle(ServiceHandle);
      end
      else
        memLog.Lines.Add(Format(FMT_LOGLINE,[
            getFmtTime, 'CHECAGEM DE SPOOLER', 'Erro ao abrir o serviço de spooler'])
          );
      // Fecha o gerenciador de serviços
      CloseServiceHandle(ServiceManager);
    end
    else
      memLog.Lines.Add(Format(FMT_LOGLINE,[
            getFmtTime, 'CHECAGEM DE SPOOLER', 'Erro ao abrir o gerenciador de serviços'])
          );
  except
    on e : exception do
      begin
        memLog.Lines.Add(Format(FMT_LOGLINE,[
            getFmtTime, 'CHECAGEM DE SPOOLER', 'Erro: '+e.Message])
          );
      end;
  end;
end;

procedure TfrmMain.CloseMe;
begin
  if MessageDlg('Deseja fechar o assistente de impressão?',TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    begin
      Application.Terminate;
    end;

end;

function TfrmMain.coalesce(aData: TArray<String>): String;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := Low(aData) to High(aData) do
    begin
      if aData[i] <> EmptyStr then
        begin
          Result := aData[i];
          Break;
        end;
    end;
end;

function TfrmMain.concatStr(aData: TArray<String>): String;
var
  i : Integer;
begin
  Result := EmptyStr;
  for i := Low(aData) to High(aData) do
    Result := Result + aData[i];
end;

procedure TfrmMain.ConectarMongo;
begin
if monoConexao <> nil then
    begin
      with mongoDB do
        begin
          Close;

          {$IFDEF WIN32}
            SpecificOptions.Values['MongoDB.BSONLibrary'] := mongoLib32+'libbson-1.0.dll';
            SpecificOptions.Values['MongoDB.ClientLibrary'] := mongoLib32+'libmongoc-1.0.dll';
          {$ELSE}
            SpecificOptions.Values['MongoDB.BSONLibrary'] := mongoLib64+'libbson-1.0.dll';
            SpecificOptions.Values['MongoDB.ClientLibrary'] := mongoLib64+'libmongoc-1.0.dll';
          {$ENDIF}


          SpecificOptions.Values['MongoDB.ConnectionFormat'] := 'cfStandard';

          Database := monoConexao.GetValue<String>('database');
          Port := 27017;
          Server := monoConexao.GetValue<String>('srv');
          Username := monoConexao.GetValue<String>('user');
          Password := monoConexao.GetValue<String>('senha');
          try
            Connect;
            OnlineServiceActive := Connected;
          except
            on e : Exception do
              begin
                memLog.Lines.Add(
                  Format(
                    FMT_LOGLINE,
                    [getFmtTime,
                    'ERRO AO CONECTAR NO BANCO ONLINE',
                    e.Message]
                  )
                )
              end;
          end;
        end;
    end;
end;

procedure TfrmMain.DesconectarMongo;
begin
  if mongoDB.InTransaction then
    mongoDB.Commit;

  mongoDB.Close;
  OnlineServiceActive := false;
end;

function TfrmMain.fmtJsonDate(aDate : String; withHour : Boolean = true) : String;
var
  date : String;
  hour : String;
  split : TStringList;
  newDate : TDateTime;
begin
  date := aDate.Substring(0, 10);
  hour := aDate.Substring(11, aDate.Length);

  split := TStringList.Create;
  split.StrictDelimiter := true;
  split.Delimiter := '-';
  split.DelimitedText := date;

  newDate := StrToDateTime(Concat(split.Strings[2],'/',split.Strings[1],'/',split.Strings[0],' ',
    hour));
  if withHour then
    Result := FormatDateTime('dd/mm/yyyy hh:mm:ss', IncHour(newDate, -3))
  else
    Result := FormatDateTime('dd/mm/yyyy', IncHour(newDate, -3));
end;

procedure TfrmMain.FormCreate(Sender: TObject);

begin
  imgLocalServiceStatus.Picture := nil;
  imgOnlineServiceStatus.Picture := nil;
  imgSpoolerStatus.Picture := nil;

  setStatusImagem(ssLocal, RED_STATUS);
  setStatusImagem(ssOnline, RED_STATUS);
  setStatusImagem(ssSpooler, RED_STATUS);

  StopService := false;
  DoingAction := false;

  switchLocalService.State := tssOn;

  memLog.Clear;

  CheckEnviorment;
end;

function TfrmMain.getAppPath: String;
begin
  Result := TPath.GetDirectoryName(ParamStr(0));
  Result := Result + PathDelim;
end;

function TfrmMain.getCDSItens: TClientDataSet;
begin
  Result := TClientDataSet.Create(Application);

  with Result do
    begin
      Close;
      FieldDefs.Clear;
      FieldDefList.Clear;

      FieldDefs.Add('NomeItem', TFieldType.ftString , 255);
      FieldDefs.Add('Qtde', TFieldType.ftFloat);
      FieldDefs.Add('VlrUnit', TFieldType.ftFloat);
      FieldDefs.Add('VlrItemTotal', TFieldType.ftFloat);

      CreateDataSet;
      Open;
    end;
end;

function TfrmMain.getCFGFile: TIniFile;
var
  ini : TStringList;
begin
  if not TFile.Exists(getAppPath+CFG_FILE) then
    begin
      ini := TStringList.Create;
      with ini do
        begin
          Clear;
          Add('['+INI_SECTION_LOCAL_CONFIG+']');
          Add(INI_OPTION_HOST+'=');
          Add(INI_OPTION_CLIENT_CNPJ+'=');
          Add(INI_OPTION_CLIENT_EMPR_ID+'=');
          Add(INI_OPTION_CLIENT_LAST_PRINT+'=');
          Add(INI_OPTION_DEFAULT_PRINTER +'=');
          Add(INI_OPTION_DEFAULT_MODEL+'=');
          Add(INI_OPTION_DEFAULT_TIMER+'=30');

          SaveToFile(getAppPath+CFG_FILE);
          Result := TIniFile.Create(getAppPath+CFG_FILE);
        end;
    end
  else
    begin
      Result := TIniFile.Create(getAppPath+CFG_FILE);
    end;
end;

function TfrmMain.getClientData(id, cnpj: String): TJSONObject;
begin
  try
    apiClient.BaseURL := Concat(cfgFile.ReadString(INI_SECTION_LOCAL_CONFIG, INI_OPTION_HOST, 'http://localhost:9090'),
    '/cliente/',
    id,'/',cnpj);
    apiRequest.Method := rmGET;
    apiRequest.Execute;

    if apiResponse.StatusCode = 200 then
      begin
        Result := TJSONObject(TJSONObject.ParseJSONValue(apiResponse.Content));
      end
    else
      Result := TJSONObject.Create;

    apiClient.Disconnect;
  except
    on e : Exception do
      begin
        memLog.Lines.Add(
          Format(
            FMT_LOGLINE,
            [getFmtTime, 'ERRO AO BUSCAR DADOS DO CLIENTE', e.Message]
          )
        )
      end;
  end;

end;

function TfrmMain.getClientData_(id, cnpj: String): TJSONObject;
begin
  try
    with mongoQry do
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
    on e : Exception do
      begin
        memLog.Lines.Add(
          Format(
            FMT_LOGLINE,
            [getFmtTime, 'ERRO AO BUSCAR DADOS DO CLIENTE', e.Message]
          )
        )
      end;
  end;
end;

function TfrmMain.getFmtTime: String;
begin
  Result := FormatDateTime('dd/mm/yyyy hh:mm:ss',now);
end;

function TfrmMain.getHost: String;
begin
  Result := cfgFile.ReadString(INI_SECTION_LOCAL_CONFIG, INI_OPTION_HOST, EmptyStr);
end;

procedure TfrmMain.getJobs;
var
  onlineJobs : TJSONObject;
  jobsWaiting : TJSONArray;
  job : TJSONValue;
begin
  LimparLog;
  DoingAction := true;
  if not mongoDB.Connected then
    ConectarMongo;

  onlineJobs := getJobsFromServer_;
  if Assigned(onlineJobs) then
    begin
      if onlineJobs.GetValue<TJSONArray>('orders').Count > 0 then
        begin
          loadToCache(onlineJobs);
        end;
    end;
  (*if onlineJobs.ToJSON <> '{}' then
    begin
      if onlineJobs.TryGetValue<TJSONArray>('jobs_waiting', jobsWaiting) then
        begin
          for job in jobsWaiting do
            begin
              updateJobStatus(job.GetValue<String>('id').ToLower.Replace('{',EmptyStr).Replace('}',EmptyStr));
              loadToCache(TJSONObject(job));
            end;
        end;
    end;*)
  if mongoDB.Connected then
    DesconectarMongo;

  DoingAction := false;
end;

function TfrmMain.getJobsFromServer: TJSONObject;
var
  lastOne : String;
  response : TStringStream;
begin
  try
  lastOne := cfgFile.ReadString(INI_SECTION_LOCAL_CONFIG, INI_OPTION_CLIENT_LAST_PRINT, '-1');

  apiClient.BaseURL := Concat(getHost, '/jobs/',
    lastOne, '/', EmprID, '/',  EmprCNPJ);
  apiRequest.Method := rmGET;
  apiRequest.Execute;
  response := TStringStream.Create('{}',TEncoding.UTF8);
  if apiResponse.StatusCode = 200 then
    begin
      response.Free;
      response := TStringStream.Create(apiResponse.Content, TEncoding.UTF8);
    end;
  Result := TJSONObject(TJSONObject.ParseJSONValue(response.DataString));
  FreeAndNil(response);
  except
    on e : exception do
      begin
        memLog.Lines.Add(
          Format(
            FMT_LOGLINE,
            [getFmtTime, 'ERRO AO BUSCAR PEDIDOS', e.Message]
          )
        )
      end;
  end;
end;

function TfrmMain.getJobsFromServer_: TJSONObject;
var
  mDoc : TMongoDocument;
  jobData : TJSONObject;

  DtoVenda : TJSONArray;
  DtoVendaProduto : TJSONArray;
  iVenda: Integer;
  LastPrinted : String;
begin
  try
    jobData := TJSONObject.Create(TJSONPair.Create('orders',TJSONValue(TJSONArray.Create)));
    LastPrinted := getCFGFile.ReadString(INI_SECTION_LOCAL_CONFIG, INI_OPTION_CLIENT_LAST_PRINT, '');

    if OnlineServiceActive and (LastPrinted.IsEmpty = false) then
      begin
        with mongoQry do
          begin
            Close;
            Sql.Clear;
            Sql.Add('{"find":"DtoVenda", "filter":{"Codigo":{$gt:'+LastPrinted+'}, '+
              '"Impresso": false, "OrigemVenda":{$in:["PDV Mobi","PDV","Venda Direta","Pedido Faturado"]}, "EmpresaId":"'+EmprID+'"}}');
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
  Result := jobData;
  except
    on e : exception do
      begin
        memLog.Lines.Add(
          Format(
            FMT_LOGLINE,
            [getFmtTime, 'ERRO AO BUSCAR PEDIDOS', e.Message]
          )
        )
      end;
  end;
end;

function TfrmMain.getOrderFromServer(cod: String): TJSONObject;
var
  response : TStringStream;
begin
  try
    apiClient.BaseURL := Concat(getHost, '/order/',
      cod, '/', EmprCNPJ, '/', EmprID );
    apiRequest.Method := rmGET;
    apiRequest.Execute;
    response := TStringStream.Create('{}',TEncoding.UTF8);
    if apiResponse.StatusCode = 200 then
      begin
        response.Free;
        response := TStringStream.Create(apiResponse.Content, TEncoding.UTF8);
      end;
    Result := TJSONObject(TJSONObject.ParseJSONValue(response.DataString));
    FreeAndNil(response);
  except
    on e : Exception do
      begin
        memLog.Lines.Add(
          Format(
            FMT_LOGLINE,
            [getFmtTime, 'ERRO AO BUSCAR PEDIDO', e.Message]
          )
        )
      end;

  end;
end;

function TfrmMain.getOrderFromServer_(cod: String): TJSONObject;
var
  mDoc : TMongoDocument;
  jobData : TJSONObject;

  DtoVenda : TJSONArray;
  DtoVendaProduto : TJSONArray;
  iVenda: Integer;
begin
  try
    jobData := TJSONObject.Create(TJSONPair.Create('orders',TJSONValue(TJSONArray.Create)));

    if OnlineServiceActive and (cod.IsEmpty=false) then
      begin
        with mongoQry do
          begin
            Close;
            Sql.Clear;
            Sql.Add('{"find":"DtoVenda", "filter":{"Codigo":' + cod +
              ', "EmpresaId":"'+EmprID+'"}}');
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

      Result := jobData;
  except
    on e : exception do
      begin
        Result := TJSONObject(TJSONObject.ParseJSONValue('{"orders":[]}'));

        memLog.Lines.Add(
          Format(
            FMT_LOGLINE,
            [getFmtTime,
            'ERRO AO BUSCAR O PEDIDO ONLINE',
            e.Message
            ]
          )
        )
      end;
  end;

end;

function TfrmMain.getOrderPayment(payments: TJSONArray): String;
const
  LINE_PAYMENT = '%s - %s - R$ %s';
var
  paymts : TStringList;
  val : TJSONValue;
  i : Integer;
begin
  paymts := TStringList.Create;
  paymts.Clear;
  paymts.LineBreak := EmptyStr;
  if Assigned(payments) then
    begin
      if payments.Count > 0 then
        begin
          for i := 0 to payments.Count-1 do
            begin
              val := payments[i];
              paymts.Add(
                Trim(Format(
                  LINE_PAYMENT,[
                    val.GetValue<String>('FormaPagamento'),
                    fmtJsonDate(val.GetValue<String>('DataTransacao.$date'), false),
                    FormatFloat('#,##0.00', val.GetValue<Extended>('ValorPagamento'))

                  ]
                ))
              );
            end;
          if payments.Count > 1 then
            paymts.LineBreak := sLineBreak;

        end;
    end;

  Result := QuotedStr(paymts.Text);
end;

function TfrmMain.getURL_HeartBeat: String;
begin
  Result := EmptyStr;
  if getHost <> EmptyStr then
    begin
      Result := concat(getHost,'/heartBeat');
    end;
end;

procedure TfrmMain.HideMe;
begin
  Self.Hide;
end;

procedure TfrmMain.LimparLog;
begin
  if memLog.Lines.Count >= 200 then
    begin
      memLog.Lines.SaveToFile(getAppPath+'bkp_log_'+
      FormatDateTime('dd_mm_yyyy_hhmmss',now)+'.log');
      memLog.Lines.Clear;
    end;
end;

procedure TfrmMain.LoadPrinters;
var
  defaultPrinter : String;
begin
  cbTicketPrinter.Items.Clear;
  cbTicketPrinter.Items.AddStrings(Printer.Printers);

  defaultPrinter := cfgFile.ReadString(INI_SECTION_LOCAL_CONFIG, INI_OPTION_DEFAULT_PRINTER, EmptyStr);

  cbTicketPrinter.ItemIndex := cbTicketPrinter.Items.IndexOf(defaultPrinter);
end;

procedure TfrmMain.LoadReports;
var
  aFile, aDefault : String;
  aReport : TplugReport;
  I: Integer;
  rel : TResourceStream;
begin
  cbPrintModel.Clear;
  if TDirectory.IsEmpty(getAppPath+REPORT_PATH) then
    begin
      rel := TResourceStream.Create(HInstance, 'REL80COL', RT_RCDATA);
      rel.SaveToFile(getAppPath+REPORT_PATH+PathDelim+'rel_80col_default.fr3');
      FreeAndNil(rel);
    end;

  for aFile in TDirectory.GetFiles(getAppPath+REPORT_PATH) do
    begin
      aReport := TplugReport.Create(aFile);
      cbPrintModel.AddItem(aReport.FileName, aReport);
    end;

  aDefault := cfgFile.ReadString(INI_SECTION_LOCAL_CONFIG, INI_OPTION_DEFAULT_MODEL, EmptyStr);

  for I := 0 to cbPrintModel.Items.Count-1 do
    begin
      if TplugReport(cbPrintModel.Items.Objects[I]).FileName = aDefault then
        begin
          cbPrintModel.ItemIndex := I;
          Break;
        end;
    end;
end;

procedure TfrmMain.loadToCache(jobs: TJSONObject);
var
  orders : TJSONArray;
  order : TJSONValue;
begin
  try
    conLocal.StartTransaction;
    with localQry do
      begin
        Close;
        Sql.Clear;
        Sql.Add('INSERT OR REPLACE INTO job_cache (job_id, job_data)');
        //Sql.Add('VALUES ('+QuotedStr(jobs.GetValue<String>('id'))+
        Sql.Add('VALUES (null'+
        //','+QuotedStr(jobs.GetValue<String>('job_data'))+');');
        ','+QuotedStr(jobs.ToJSON)+');');
        ExecSQL;
      end;
    conLocal.Commit;

  //strRead := TStringStream.Create(jobs.GetValue<String>('job_data'), TEncoding.UTF8);
  orders := jobs.GetValue<TJSONArray>('orders');
  for order in orders do
    begin
      printOrder(TJSONObject(order));
    end;
  updateLastOne;
  except
    on e : exception do
      begin
        memLog.Lines.Add(
          Format(
            FMT_LOGLINE,[
              getFmtTime,
              //'Erro ao gravar o trabalho('+QuotedStr(jobs.GetValue<String>('id'))+') no cache',
              'Erro ao gravar o trabalho no cache',
              e.Message
            ]
          )
        );
      end;
  end;

end;

procedure TfrmMain.LocalCache;
var
  _localCache : TResourceStream;
  database : String;
begin
  database := getAppPath+CLIENT_PATH+PathDelim+'localCache.db';
  if not TFile.Exists(database) then
    begin
      _localCache := TResourceStream.Create(HInstance, 'LOCAL_CACHE', RT_RCDATA);
      _localCache.SaveToFile(database);
    end;

  with conLocal do
    begin
      Close;
      Params.Database := database;
      Connected := true;

      if Connected then
        begin
          memLog.Lines.Add(
            Format(FMT_LOGLINE, [
              getFmtTime,
              'CACHE CONECTADO',
              database
            ])
          );
        end;
    end;
end;

procedure TfrmMain.mongoEnv;
var
  aResource : TResourceStream;
  aZip : TZipFile;
begin
    {$IFDEF WIN32}
      if not TDirectory.Exists(mongoLib32) then
      TDirectory.CreateDirectory(mongoLib32);
      if not TFile.Exists(mongoLib32+'libmongoc-1.0.dll') then
        begin
          if Assigned(aResource) then
            FreeAndNil(aResource);

          aResource := TResourceStream.Create(hInstance, 'MONGO32', RT_RCDATA);
          aZip := TZipFile.Create;
          aZip.Open(aResource, zmRead);
          aZip.ExtractAll(mongoLib32);
        end;
    {$ELSE}
      if not TDirectory.Exists(mongoLib64) then
        TDirectory.CreateDirectory(mongoLib64);
      if not TFile.Exists(mongoLib64+'libmongoc-1.0.dll') then
        begin
          if Assigned(aResource) then
            FreeAndNil(aResource);

          aResource := TResourceStream.Create(hInstance, 'MONGO64', RT_RCDATA);
          aZip := TZipFile.Create;
          aZip.Open(aResource, zmRead);
          aZip.ExtractAll(mongoLib64);
        end;
    {$ENDIF}
end;

function TfrmMain.mongoLib32: String;
begin
  Result := getAppPath+'lib'+PathDelim+'mongo32'+PathDelim;
end;

function TfrmMain.mongoLib64: String;
begin
  Result := getAppPath+'lib'+PathDelim+'mongo64'+PathDelim;
end;

procedure TfrmMain.popCloseSystemClick(Sender: TObject);
begin
  CloseMe;
end;

procedure TfrmMain.popShowFormClick(Sender: TObject);
begin
  ShowMe;
end;

procedure TfrmMain.printOrder(order: TJSONObject);
var
  cdsItens : TClientDataSet;
  order_item : TJSONValue;
  cliente : TJSONObject;
  strTeste, orderID, orderDate, orderClient : String;

  printStart, printFinish : TDateTime;
begin
  if order <> nil then
    begin
      printStart := now;
      if btReimprimir.Enabled then
        saveOrderToCache(order.GetValue<String>('DtoVenda.Codigo.$numberLong'),
          order.GetValue<TJSONObject>('DtoVenda').ToJSON,
          order.GetValue<TJSONArray>('DtoVendaProduto').ToJSON);

      cdsItens := getCDSItens;
      for order_item in order.GetValue<TJSONArray>('DtoVendaProduto') do
        begin
          cdsItens.Append;
          cdsItens.FieldByName('NomeItem').Value := order_item.GetValue<String>('Descricao');
          cdsItens.FieldByName('Qtde').Value := order_item.GetValue<Extended>('Quantidade');
          cdsItens.FieldByName('VlrUnit').Value := order_item.GetValue<Extended>('ValorUnitario');
          cdsItens.FieldByName('VlrItemTotal').Value := order_item.GetValue<Extended>('ValorTotal');
          cdsItens.Post;
        end;

      repData.DataSet := cdsItens;
      with reports do
        begin
          Clear;
          LoadFromFile(TplugReport(cbPrintModel.Items.Objects[cbPrintModel.ItemIndex]).FullPath);
          Variables['FILIAL_NAME'] := QuotedStr(thisFilial.GetValue<String>('RazaoSocial'));
          Variables['FILIAL_ADDRESS'] := QuotedStr(concatStr([
            thisFilial.GetValue<String>('Logradouro'),', ',
            thisFilial.GetValue<String>('Numero')
          ]));
          Variables['FILIAL_CITY'] := QuotedStr(concatStr([
            thisFilial.GetValue<String>('Bairro'),' - ',
            thisFilial.GetValue<String>('Cidade'),'-',
            thisFilial.GetValue<String>('UF')
          ]));
          Variables['FILIAL_PHONE'] := QuotedStr(thisFilial.GetValue<String>('Telefone'));

          orderID := concat(order.GetValue<String>('DtoVenda._id.$oid').ToLower,
           ' (',order.GetValue<String>('DtoVenda.Codigo.$numberLong'),') ');
          Variables['ORDER_ID'] := QuotedStr(order.GetValue<String>('DtoVenda.Codigo.$numberLong'));
          if order.GetValue<TJSONValue>('DtoVenda.PagamentosVenda') is TJSONNull then
            Variables['ORDER_PAYMENT'] := QuotedStr('Forma de Pgto não informada')
          else
            Variables['ORDER_PAYMENT'] := getOrderPayment(order.GetValue<TJSONArray>('DtoVenda.PagamentosVenda'));

          //Chamar o Cliente aqui
          cliente := getClientData_(order.GetValue<String>('DtoVenda.ClienteId'),
            cfgFile.ReadString(INI_SECTION_LOCAL_CONFIG, INI_OPTION_CLIENT_CNPJ, ''));

          if cliente.TryGetValue<String>('Cliente', strTeste) then
            begin
              orderClient := coalesce([cliente.GetValue<String>('NomeFantasia'),
              cliente.GetValue<String>('RazaoSocial')]);
              Variables['ORDER_CLIENT'] := QuotedStr(orderClient);

              Variables['ORDER_CNPJ'] := QuotedStr(cliente.GetValue<String>('CNPJ_CPF'));
              Variables['ORDER_CLI_ADDRESS'] := QuotedStr(concatStr([
                 cliente.GetValue<String>('Logradouro'),', ',
                 cliente.GetValue<String>('LogradouroNumero'),' - ',
                 cliente.GetValue<String>('Bairro')
              ]));
              Variables['ORDER_PHONE'] := QuotedStr(cliente.GetValue<String>('Telefone'));
            end;

          orderDate := fmtJsonDate(order.GetValue<TJSONValue>('DtoVenda.Data').GetValue<String>('$date'));
          Variables['ORDER_DATE'] := QuotedStr(orderDate);

          Variables['ORDER_VLRTOTAL'] := order.GetValue<Extended>('DtoVenda.ValorFinal');
          Variables['ORDER_SHIPTOTAL'] := order.GetValue<Extended>('DtoVenda.ValorFrete');
          Variables['ORDER_DESCTOTAL'] := order.GetValue<Extended>('DtoVenda.DescontoDinheiro');

          PrintOptions.ShowDialog := false;
          PrintOptions.Printer := cbTicketPrinter.Text;
          PrintOptions.PrintMode := TfrxPrintMode.pmDefault;
          PrintOptions.Copies := 1;

          PrepareReport(true);
          Print;
          printFinish := now;
          memLog.Lines.Add(Format(FMT_LOGLINE,[
            getFmtTime+' - Imprimindo Pedido: '+orderID,
            'Cliente: '+orderClient+' | Data: '+orderDate,
            'Tempo gasto: '+SecondsBetween(printStart, printFinish).ToString+' seg.'
          ]));
        end;
    end;
end;

procedure TfrmMain.saveOrderToCache(id, order, itens: String);
begin
  try
    conLocal.StartTransaction;
    with localQry do
      begin
        Close;
        Sql.Clear;
        Sql.Add('INSERT OR REPLACE INTO orders (order_id, order_json, order_itens_json)');
        Sql.Add('VALUES ('+id+','+QuotedStr(order)+','+QuotedStr(itens)+')');
        ExecSQL;
      end;
    conLocal.Commit;
  except
    on e : exception do
      begin
        memLog.Lines.Add(
          Format(
            FMT_LOGLINE,[
              getFmtTime,
              'Erro ao gravar a Venda ('+id+') no cache',
              e.Message
            ]
          )
        );
      end;
  end;
end;

procedure TfrmMain.serviceTimer(Sender: TObject);
begin
  if StopService then
    begin
      service.Enabled := false;
    end
  else
    begin
      if not DoingAction then
        begin
          getJobs;
        end;
    end;
end;

procedure TfrmMain.SetFilial;
var
  clients : TStringStream;
  jClients : TJSONArray;
  filial : TJSONValue;
  thisCNPJ : String;
  fileFilial : TResourceStream;
begin
  if not TFile.Exists(getAppPath+CLIENT_PATH+PathDelim+FILIAL_FILE) then
    begin
      fileFilial := TResourceStream.Create(HInstance, 'JSONFILIAIS', RT_RCDATA);
      fileFilial.SaveToFile(getAppPath+CLIENT_PATH+PathDelim+FILIAL_FILE);
      FreeAndNil(fileFilial);
    end;

  clients := TStringStream.Create('',TEncoding.UTF8);
  clients.LoadFromFile(getAppPath+CLIENT_PATH+PathDelim+FILIAL_FILE);
  jClients := TJSONObject.ParseJSONValue(clients.DataString).GetValue<TJSONArray>('filial');
  thisCNPJ := cfgFile.ReadString(INI_SECTION_LOCAL_CONFIG, INI_OPTION_CLIENT_CNPJ, EmptyStr);

  for filial in jClients do
    begin
      if filial.GetValue<String>('CNPJ').Equals(thisCNPJ) then
        begin
          thisFilial := TJSONObject(filial);
          EmprCNPJ := thisCNPJ;
          break;
        end;
    end;
  if thisFilial <> nil then
    begin
      cfgFile.WriteString(INI_SECTION_LOCAL_CONFIG, INI_OPTION_CLIENT_EMPR_ID, thisFilial.GetValue<String>('_id'));
      EmprID := thisFilial.GetValue<String>('_id');
      memLog.Lines.Add(
        Format(FMT_LOGLINE, [
          getFmtTime,
          'EMPRESA CARREGADA',
          thisFilial.GetValue<String>('CNPJ')+' -> '+thisFilial.GetValue<String>('RazaoSocial')
        ])
      );

      monoConexao := thisFilial.GetValue<TJSONValue>('Conexao');
    end
  else
    begin
      memLog.Lines.Add(
        Format(FMT_LOGLINE, [
          getFmtTime,
          'EMPRESA NÃO CARREGADA',
          'CNPJ CONFIGURADO: '+thisCNPJ
        ])
      );
      monoConexao := TJSONObject.ParseJSONValue('{"srv":"","user":"","senha":"","database":"","usuaid":"","usuaemail":""}');
    end;
end;

procedure TfrmMain.setStatusImagem(service: TSvcStatus; image: String);
var
  imgRes : TResourceStream;
begin
  imgRes := TResourceStream.Create(hInstance, image, RT_RCDATA);
  case service of
    ssLocal: imgLocalServiceStatus.Picture.LoadFromStream(imgRes);
    ssOnline: imgOnlineServiceStatus.Picture.LoadFromStream(imgRes);
    ssSpooler: imgSpoolerStatus.Picture.LoadFromStream(imgRes);
  end;
  FreeAndNil(imgRes);
end;

procedure TfrmMain.ShowMe;
begin
  Self.Show;
end;

procedure TfrmMain.switchLocalServiceClick(Sender: TObject);
begin
  StopService := (switchLocalService.State = tssOff);
  if not StopService then
    DoingAction := false;
end;

procedure TfrmMain.trayIconDblClick(Sender: TObject);
begin
  ShowMe;
end;

procedure TfrmMain.updateDtoVenda(id: String);
begin
  try
  with mongoCMD do
    begin
      Close;
      Sql.Clear;
      Sql.Text := '{"update":"DtoVenda", "updates":[{"q":{"_id": {"$oid":"'+id+'"}},"u":{"$set":{"Impresso":true}}}]}';
      ExecSQL;
    end;
  except
    on e : exception do
      begin
        memLog.Lines.Add(
          Format(
            FMT_LOGLINE,
            [getFmtTime,
            'ERRO AO ATUALIZAR A VENDA PARA IMPRESSO',
            e.Message]
          )
        )
      end;

  end;
end;

procedure TfrmMain.updateJobStatus(id: String);
var
  response, dtUpdte : TJSONValue;
begin
  apiClient.BaseURL := concat(getHost, '/updateJob');
  apiRequest.Method := rmPOST;
  apiRequest.Body.ClearBody;
  apiRequest.Body.Add(
    TJSONObject.Create.AddPair(
      TJSONPair.Create('id',id)
    ).AddPair('cnpj',EmprCNPJ)
  );
  apiRequest.Execute;
  if apiResponse.StatusCode = 200 then
    begin
      if TJSONObject(TJSONObject.ParseJSONValue(apiResponse.Content)).
        TryGetValue<TJSONValue>('error',response)  then
        begin
          memLog.Lines.Add(
            Format(
              FMT_LOGLINE, [
                getFmtTime,
                'Falha ao Processar o Trabalho: '+id,
                response.GetValue<String>('exception_message')
              ]
            )
          );
        end
      else
        begin
          TJSONObject(TJSONObject.ParseJSONValue(apiResponse.Content)).
            TryGetValue<TJSONValue>('jobs_waiting',response);
          if TJSONArray(response).Count > 0 then
            begin
              for dtUpdte in TJSONArray(response) do
                begin
                  memLog.Lines.Add(
                    Format(
                      FMT_LOGLINE, [
                        getFmtTime,
                        'Processando o Trabalho: '+id,
                        dtUpdte.GetValue<String>('job_collected_at')
                      ]
                    )
                  );
                end;
            end;
        end;
    end;

end;

procedure TfrmMain.updateLastOne;
begin
  with localQry do
    begin
      Close;
      Sql.Clear;
      Sql.Add('SELECT MAX(order_id) as LastOne FROM orders');
      Open;
      if not IsEmpty then
        begin
          cfgFile.WriteInteger(INI_SECTION_LOCAL_CONFIG, INI_OPTION_CLIENT_LAST_PRINT,
          FieldByName('LastOne').AsInteger);
        end;
    end;
end;

{ TplugReport }

constructor TplugReport.Create(aFullPath: String);
begin
  inherited Create;
  Self.FFullPath := aFullPath;
  Self.FExt := TPath.GetExtension(aFullPath);
  Self.FFileName := TPath.GetFileNameWithoutExtension(aFullPath);
end;

end.
