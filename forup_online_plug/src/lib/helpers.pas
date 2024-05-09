unit helpers;

interface
uses System.IOUtils, System.StrUtils, System.WideStrUtils, System.Math,
System.DateUtils, System.Classes, System.SysUtils, Generics.Collections,
forup_types, System.Zip, System.Types;

type
  Tfunc_helper = class(TObject)
    private
      procedure CreateDBConfig;
      procedure CreateConfigFile(typeConfig : Tcfg_type);
      procedure LoadResource;
    public
      function AppPath : String;
      function InitEnv : Boolean;

      function item_in_array(item : Integer; aArray : TArray<Integer>) : Boolean; overload;
      function item_in_array(item : String; aArray : TArray<String>) : Boolean; overload;

      function prepare_string_json(aStr : String) : String;
  end;

  THelper = class
    strict private
      class var FunctionHelper : Tfunc_helper;
    public
      class function Functions : Tfunc_helper;
  end;

implementation

{ Tfunc_helper }

function Tfunc_helper.appPath: String;
var
  app : String;
begin
  app := TPath.GetDirectoryName(ParamStr(0));
  Result := app + PathDelim;
end;

procedure Tfunc_helper.CreateConfigFile(typeConfig: Tcfg_type);
var
  dir : String;
  alogFile : TextFile;
begin
  case typeConfig of
    ctDataBase : begin
      dir := AppPath + CONN_CFG_DIR;
      if not TDirectory.Exists(dir) then
        TDirectory.CreateDirectory(dir);

      if not TFile.Exists(dir+CONN_CFG_FILE) then
        CreateDBConfig;
    end;
    ctResource : begin
      LoadResource;
    end;
    ctService : begin

    end;
    ctOther : begin
      dir := AppPath + LOGDIR;
      if not TDirectory.Exists(dir) then
        TDirectory.CreateDirectory(dir);

      if not TFile.Exists(dir+LOGFILE) then
        begin
          Assign(alogFile, dir+LOGFILE);
          Rewrite(alogFile);
          CloseFile(alogFile);
        end;
    end;
  end;
end;

procedure Tfunc_helper.CreateDBConfig;
var
  aFile : TStringList;
  fName : String;
begin
  aFile := TStringList.Create;
  fName := AppPath + CONN_CFG_DIR + CONN_CFG_FILE;
  aFile.Add('[POSTGRE]');
  aFile.Add('DriverID=PG');
  aFile.Add('Server=localhost');
  aFile.Add('User_Name=plugupservice');
  aFile.Add('Password=ForUp@s3rv1c0');
  aFile.Add('Port=5432');
  aFile.Add('Database=plugupservice');
  aFile.Add('CharacterSet=utf8');
  aFile.Add('MetaDefSchema=jobservice');
  aFile.Add('ExtendedMetadata=True');
  aFile.Add('');
  aFile.Add('[MYSQL]');
  aFile.Add('');
  aFile.Add('[SYBASE]');
  aFile.Add('');
  aFile.Add('[FIREBIRD]');
  aFile.Add('');
  aFile.Add('[ORACLE]');
  aFile.Add('');
  aFile.Add('[SQLITE]');
  aFile.Add('');
  aFile.Add('[SQLSERVER]');
  aFile.SaveToFile(fName);

  fName := AppPath + CONN_CFG_DIR + 'drv.ini';
  aFile.Clear;
  aFile.Add('[FDDrivers.ini]');
  aFile.Add('Encoding=UTF8');
  aFile.SaveToFile(fName);
end;

function Tfunc_helper.InitEnv: Boolean;
begin
  CreateConfigFile(ctDataBase);
  CreateConfigFile(ctResource);

  CreateConfigFile(ctOther);

  Result := TFile.Exists(AppPath+CONN_CFG_DIR+CONN_CFG_FILE);
end;

function Tfunc_helper.item_in_array(item: String;
  aArray: TArray<String>): Boolean;
var
  iArray: Integer;
begin
  Result := false;
  for iArray := Low(aArray) to High(aArray) do
    begin
      if item = aArray[iArray] then
        begin
          Result := true;
          Break;
        end;
    end;
end;

procedure Tfunc_helper.LoadResource;
var
  aResource: TResourceStream;
  aZip : TZipFile;
begin
  aResource := TResourceStream.Create(hInstance, 'PGDRV', RT_RCDATA);
  if not TDirectory.Exists(AppPath+CONN_CFG_DIR+'drv') then
    TDirectory.CreateDirectory(AppPath+CONN_CFG_DIR+'drv');

  aZip := TZipFile.Create;
  aZip.Open(aResource, zmRead);
  aZip.ExtractAll(AppPath+CONN_CFG_DIR+'drv'+PathDelim);
end;

function Tfunc_helper.prepare_string_json(aStr: String): String;
begin
  Result := StringReplace(aStr,'"','\"',[rfReplaceAll]);
end;

function Tfunc_helper.item_in_array(item: Integer;
  aArray: TArray<Integer>): Boolean;
var
  iArray: Integer;
begin
  Result := false;
  for iArray := Low(aArray) to High(aArray) do
    begin
      if item = aArray[iArray] then
        begin
          Result := true;
          Break;
        end;
    end;
end;

{ THelper }

class function THelper.Functions: Tfunc_helper;
begin
  if not Assigned(Self.FunctionHelper) then
    Self.FunctionHelper := Tfunc_helper.Create;

  Result := Self.FunctionHelper;
end;

end.
