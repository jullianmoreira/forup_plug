unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus, Vcl.Buttons,
  Vcl.Imaging.pngimage, Vcl.StdCtrls, System.ImageList, Vcl.ImgList, System.UITypes,
  Vcl.WinXCtrls, Vcl.Samples.Spin, System.IniFiles, System.IOUtils, Job_Executor;

const
  CFG_FILE = 'cfg.ini';

  INI_SECTION_LOCAL_CONFIG = 'LOCAL CONFIG';
  INI_OPTION_CLIENT_CNPJ = 'CLIENT_CNPJ';
  INI_OPTION_CLIENT_EMPR_ID = 'CLIENT_EMPR_ID';
  INI_OPTION_CLIENT_LAST_PRINT = 'CLIENT_LAST_PRINT';
  INI_OPTION_DEFAULT_PRINTER = 'DEFAULT_PRINTER';
  INI_OPTION_DEFAULT_MODEL = 'DEFAULT_MODEL';


type
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
    procedure popShowFormClick(Sender: TObject);
    procedure trayIconDblClick(Sender: TObject);
    procedure btCloseFormClick(Sender: TObject);
    procedure popCloseSystemClick(Sender: TObject);
    procedure btMinimizeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function getAppPath : String;
    function getCFGFile : TIniFile;

    procedure CloseMe;
    procedure ShowMe;
    procedure HideMe;

    procedure CheckEnviorment;
    procedure LoadPrinters;
    procedure CheckSpooler;
    procedure CheckOnlineService;
  public
    { Public declarations }
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

procedure TfrmMain.CheckEnviorment;
var
  cfgFile : TIniFile;
begin
  cfgFile := getCFGFile;
end;

procedure TfrmMain.CheckOnlineService;
begin

end;

procedure TfrmMain.CheckSpooler;
begin

end;

procedure TfrmMain.CloseMe;
begin
  if MessageDlg('Deseja fechar o assistente de impressão?',TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    begin
      Application.Terminate;
    end;

end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  imgRes : TResourceStream;
begin
  imgLocalServiceStatus.Picture := nil;
  imgOnlineServiceStatus.Picture := nil;
  imgSpoolerStatus.Picture := nil;

  imgRes := TResourceStream.Create(hInstance, 'GREENSTS', RT_RCDATA);
  imgLocalServiceStatus.Picture.LoadFromStream(imgRes);
  imgRes.Position := 0;
  imgOnlineServiceStatus.Picture.LoadFromStream(imgRes);
  imgRes.Position := 0;
  imgSpoolerStatus.Picture.LoadFromStream(imgRes);

  memLog.Clear;
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
          Add(INI_OPTION_CLIENT_CNPJ+'=');
          Add(INI_OPTION_CLIENT_EMPR_ID+'=');
          Add(INI_OPTION_CLIENT_LAST_PRINT+'=');
          Add(INI_OPTION_DEFAULT_PRINTER +'=');
          Add(INI_OPTION_DEFAULT_MODEL+'=');

          SaveToFile(getAppPath+CFG_FILE);
          Result := TIniFile.Create(getAppPath+CFG_FILE);
        end;
    end
  else
    begin
      Result := TIniFile.Create(getAppPath+CFG_FILE);
    end;
end;

procedure TfrmMain.HideMe;
begin
  Self.Hide;
end;

procedure TfrmMain.LoadPrinters;
begin

end;

procedure TfrmMain.popCloseSystemClick(Sender: TObject);
begin
  CloseMe;
end;

procedure TfrmMain.popShowFormClick(Sender: TObject);
begin
  ShowMe;
end;

procedure TfrmMain.ShowMe;
begin
  Self.Show;
end;

procedure TfrmMain.trayIconDblClick(Sender: TObject);
begin
  ShowMe;
end;

end.
