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
  FireDAC.Phys.SQLiteWrapper.Stat;

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
    procedure popShowFormClick(Sender: TObject);
    procedure trayIconDblClick(Sender: TObject);
    procedure btCloseFormClick(Sender: TObject);
    procedure popCloseSystemClick(Sender: TObject);
    procedure btMinimizeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure switchLocalServiceClick(Sender: TObject);
    procedure serviceTimer(Sender: TObject);
    procedure btUpdateConfigClick(Sender: TObject);
  private
    { Private declarations }
    checkThread : TServiceChecker;
    thisFilial : TJSONObject;

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
  public
    { Public declarations }
    cfgFile : TIniFile;

    procedure getJobs;
    procedure loadToCache(jobs : TJSONObject);
    procedure printOrder(order : TJSONObject);

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
    MessageDlg('Configura��o Salva!',TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
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
          getFmtTime, 'CHECAGEM DO HOST', 'Sem configura��o de Host do Servi�o Online'
        ]
      ));
    end
  else
    begin
      // Verifica se h� conex�o com a internet local
      dwAccessType := INTERNET_OPEN_TYPE_DIRECT;
      hInternet := InternetOpen(nil, dwAccessType, nil, nil, 0);
      if hInternet <> nil then
      begin
        // Abre uma conex�o com o URL especificado
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
          setStatusImagem(ssOnline, RED_STATUS); // O servi�o n�o est� respondendo
        InternetCloseHandle(hInternet);
      end
      else
        memLog.Lines.Add(Format(
        FMT_LOGLINE, [
          getFmtTime, 'CHECAGEM DO HOST', 'Sem conex�o com a internet'
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
    // Abre o servi�o de spooler
    ServiceHandle := OpenService(ServiceManager, 'Spooler', SERVICE_QUERY_STATUS);
    if ServiceHandle <> 0 then
    begin
      // Verifica o status do servi�o
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
          getFmtTime, 'CHECAGEM DE SPOOLER', 'Erro ao consultar o status do Spooler de Impress�o'])
        );
      // Fecha o handle do servi�o
      CloseServiceHandle(ServiceHandle);
    end
    else
      memLog.Lines.Add(Format(FMT_LOGLINE,[
          getFmtTime, 'CHECAGEM DE SPOOLER', 'Erro ao abrir o servi�o de spooler'])
        );
    // Fecha o gerenciador de servi�os
    CloseServiceHandle(ServiceManager);
  end
  else
    memLog.Lines.Add(Format(FMT_LOGLINE,[
          getFmtTime, 'CHECAGEM DE SPOOLER', 'Erro ao abrir o gerenciador de servi�os'])
        );
end;

procedure TfrmMain.CloseMe;
begin
  if MessageDlg('Deseja fechar o assistente de impress�o?',TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    begin
      Application.Terminate;
    end;

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

function TfrmMain.getFmtTime: String;
begin
  Result := FormatDateTime('dd/mm/yyyy hh:mm:ss',now);
end;

procedure TfrmMain.getJobs;
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
      Result := host+'/heartBeat';
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
begin

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
              'EMPRESA N�O CARREGADA',
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
