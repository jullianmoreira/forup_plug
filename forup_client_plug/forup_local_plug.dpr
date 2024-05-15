program forup_local_plug;

{$R *.dres}

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {frmMain},
  Job_Executor in 'Job_Executor.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
