unit helpers;

interface
uses System.IOUtils, System.StrUtils, System.WideStrUtils, System.Math,
System.DateUtils, System.Classes, System.SysUtils, Generics.Collections,
forup_types;

type
  Tfunc_helper = class(TObject)
    private
      procedure CreateConfigFile(typeConfig : Tcfg_type);
    public
      function AppPath : String;
      function InitEnv : Boolean;

      function item_in_array(item : Integer; aArray : TArray<Integer>) : Boolean; overload;
      function item_in_array(item : String; aArray : TArray<String>) : Boolean; overload;
  end;

implementation

{ Tfunc_helper }

function Tfunc_helper.appPath: String;
var
  app : String;
begin
  app := TPath.GetDirectoryName(ParamStr(0));
  Result := TPath.Combine(app, PATH_SEP);
end;

procedure Tfunc_helper.CreateConfigFile(typeConfig: Tcfg_type);
begin
  case typeConfig of
    ctDataBase : begin
      if not TDirectory.Exists(TPath.Combine(AppPath, CONN_CFG_FILE)) then
        TDirectory.CreateDirectory(TPath.Combine(AppPath, CONN_CFG_FILE));


    end;
    ctResource : begin

    end;
    ctService : begin

    end;
    ctOther : begin

    end;
  end;
end;

function Tfunc_helper.InitEnv: Boolean;
begin
  Result := false;
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

end.
