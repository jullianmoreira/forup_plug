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
  Data.Bind.Components, Data.Bind.ObjectScope, System.DateUtils;

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

    function concatStr(aData : TArray<String>) : String;
    function coalesce(aData : TArray<String>) : String;
    function fmtJsonDate(aDate : String) : String;

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
  public
    { Public declarations }
    cfgFile : TIniFile;

    procedure getJobs;
    procedure loadToCache(jobs : TJSONObject);
    procedure printOrder(order : TJSONObject);

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
begin
  cfgFile := getCFGFile;

  if not TDirectory.Exists(getAppPath+CLIENT_PATH) then
    TDirectory.CreateDirectory(getAppPath+CLIENT_PATH);
  if not TDirectory.Exists(getAppPath+REPORT_PATH) then
    TDirectory.CreateDirectory(getAppPath+REPORT_PATH);


  LoadPrinters;
  LoadReports;

  SetFilial;

  LocalCache;

  timerVal := cfgFile.ReadInteger(INI_SECTION_LOCAL_CONFIG, INI_OPTION_DEFAULT_TIMER, 30);
  service.Interval := timerVal*1000;
  spPrintInterval.Value := timerVal;
  service.Enabled := true;

  checkThread := TServiceChecker.Create(
    @LocalServiceActive, @OnlineServiceActive, @SpoolerActive,
    CallBackLocalService, CallBackOnlineService, CallBackSpooler
  );
  checkThread.OnlineChecker := CheckOnlineService;
  checkThread.SpoolerChecker := CheckSpooler;
  checkThread.LocalChecker := CheckLocarService;

  checkThread.Start;
end;

procedure TfrmMain.CheckLocarService;
begin
  LocalServiceActive := false;
  LocalServiceActive := service.Enabled;
end;

procedure TfrmMain.CheckOnlineService;
var
  hInternet : Pointer;
  hConnect: Pointer;
  dwAccessType: DWORD;
  dwFlags: DWORD;
  lpszUserAgent: LPCTSTR;
  URL : String;
begin
  OnlineServiceActive := false;
  URL := getURL_HeartBeat;
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
        lpszUserAgent := nil;
        hConnect := InternetOpenUrl(hInternet, PChar(URL), nil, 0, dwFlags, 0);
        if hConnect <> nil then
        begin
          setStatusImagem(ssOnline, GREEN_STATUS);
          OnlineServiceActive := true;
          InternetCloseHandle(hConnect);
        end
        else
          setStatusImagem(ssOnline, RED_STATUS); // O serviço não está respondendo
        InternetCloseHandle(hInternet);
      end
      else
        memLog.Lines.Add(Format(
        FMT_LOGLINE, [
          getFmtTime, 'CHECAGEM DO HOST', 'Sem conexão com a internet'
        ]
      ));
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
  i, icount : Integer;
begin
  icount := Length(aData);
  Result := EmptyStr;
  for i := Low(aData) to High(aData) do
    Result := Result + aData[i];
end;

function TfrmMain.fmtJsonDate(aDate: String): String;
var
  date : String;
  hour : String;
  split : TStringList;
begin
  date := aDate.Substring(0, 10);
  hour := aDate.Substring(11, aDate.Length);

  split := TStringList.Create;
  split.StrictDelimiter := true;
  split.Delimiter := '-';
  split.DelimitedText := date;

  Result := Concat(split.Strings[2],'/',split.Strings[1],'/',split.Strings[0],' ',
  hour);
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

end;

function TfrmMain.getFmtTime: String;
begin
  Result := FormatDateTime('dd/mm/yyyy hh:mm:ss',now);
end;

procedure TfrmMain.getJobs;
begin

end;

function TfrmMain.getJobsFromServer: TJSONObject;
begin

end;

function TfrmMain.getURL_HeartBeat: String;
var
  host : String;
begin
  host := cfgFile.ReadString(INI_SECTION_LOCAL_CONFIG, INI_OPTION_HOST, EmptyStr);
  Result := EmptyStr;
  if host <> EmptyStr then
    begin
      Result := concat(host,'/heartBeat');
    end;
end;

procedure TfrmMain.HideMe;
begin
  Self.Hide;
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
begin
  cbPrintModel.Clear;
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
begin

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

          //Chamar o Cliente aqui
          cliente := getClientData(order.GetValue<String>('DtoVenda.ClienteId'),
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

        end;
    end;
end;

procedure TfrmMain.SetFilial;
var
  clients : TStringStream;
  jClients : TJSONArray;
  filial : TJSONValue;
  thisCNPJ : String;
begin
  if TFile.Exists(getAppPath+CLIENT_PATH+PathDelim+FILIAL_FILE) then
    begin
      clients := TStringStream.Create('',TEncoding.UTF8);
      clients.LoadFromFile(getAppPath+CLIENT_PATH+PathDelim+FILIAL_FILE);
      jClients := TJSONObject.ParseJSONValue(clients.DataString).GetValue<TJSONArray>('filial');
      thisCNPJ := cfgFile.ReadString(INI_SECTION_LOCAL_CONFIG, INI_OPTION_CLIENT_CNPJ, EmptyStr);

      for filial in jClients do
        begin
          if filial.GetValue<String>('CNPJ').Equals(thisCNPJ) then
            begin
              thisFilial := TJSONObject(filial);
              break;
            end;
        end;
      if thisFilial <> nil then
        begin
          cfgFile.WriteString(INI_SECTION_LOCAL_CONFIG, INI_OPTION_CLIENT_EMPR_ID, thisFilial.GetValue<String>('_id'));
          memLog.Lines.Add(
            Format(FMT_LOGLINE, [
              getFmtTime,
              'EMPRESA CARREGADA',
              thisFilial.GetValue<String>('CNPJ')+' -> '+thisFilial.GetValue<String>('RazaoSocial')
            ])
          );
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
        end;
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

procedure TfrmMain.btReimprimirClick(Sender: TObject);
var
  jobs_waiting, orders : TJSONArray;
  job, order : TJSONValue;
  job_data : TJSONObject;
  strRead : TStringStream;
begin
  strRead := TStringStream.Create(EmptyStr, TEncoding.UTF8);
  strRead.LoadFromFile(concat(getAppPath, PathDelim, CLIENT_PATH, PathDelim, 'jobservice.json'));

  jobs_waiting := TJSONObject.ParseJSONValue(strRead.DataString).GetValue<TJSONArray>('jobs_waiting');
  for job in jobs_waiting do
    begin
      //updateJOB
      job_data := TJSONObject(TJSONObject.ParseJSONValue(job.GetValue<String>('job_data')));
      orders := job_data.GetValue<TJSONArray>('orders');

      for order in orders do
        begin
          printOrder(TJSONObject(order));
        end;
    end;

end;

procedure TfrmMain.switchLocalServiceClick(Sender: TObject);
begin
  StopService := (switchLocalService.State = tssOff);

end;

procedure TfrmMain.trayIconDblClick(Sender: TObject);
begin
  ShowMe;
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
