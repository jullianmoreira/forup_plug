program forup_plug_svc;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Uconn_module in 'connection\Uconn_module.pas' {conn_module: TDataModule},
  forup_types in 'lib\forup_types.pas',
  entity_base in 'entities\entity_base.pas',
  helpers in 'lib\helpers.pas',
  repository_base in 'repositories\repository_base.pas',
  controller_base in 'controllers\controller_base.pas',
  logger in 'lib\logger.pas';

var
  fHelper : Tfunc_helper;

begin
  fHelper := Tfunc_helper.Create;
  conn_module := Tconn_module.Create(nil);
  try
    Writeln(fHelper.AppPath);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
