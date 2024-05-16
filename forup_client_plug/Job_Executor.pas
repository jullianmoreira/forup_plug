unit Job_Executor;

interface
uses System.SysUtils, System.Classes, System.Threading, Data.DB, FireDAC.Comp.Client,
System.UITypes, System.JSON;

var
  LocalServiceActive, OnlineServiceActive, SpoolerActive : Boolean;

type
  TOutThreadProcedure = procedure of object;
  {$M+}
  TServiceChecker = class(TThread)
    private
      FLocalSvcChk: PBoolean;
      FOnlineSvcChk: PBoolean;
      FSpoolerChk: PBoolean;
      FLocalCallback: TProc<Boolean>;
      FOnlineCallback: TProc<Boolean>;
      FSpoolerCallback: TProc<Boolean>;
      FOnlineChecker: TOutThreadProcedure;
      FSpoolerChecker: TOutThreadProcedure;
      FLocalChecker: TOutThreadProcedure;
    protected
      procedure Execute; override;
    public
      constructor Create(aLocalSvc, aOnlineSvc, aSpooler: PBoolean; LocalCallBack,
        OnlineCallBack, SpoolerCallBack: TProc<Boolean>);
    published
      property OnlineChecker: TOutThreadProcedure read FOnlineChecker write FOnlineChecker;
      property SpoolerChecker: TOutThreadProcedure read FSpoolerChecker write FSpoolerChecker;
      property LocalChecker: TOutThreadProcedure read FLocalChecker write FLocalChecker;
  end;
  Tjob = class(TThread)

  end;

implementation


{ TServiceChecker }

constructor TServiceChecker.Create(aLocalSvc, aOnlineSvc, aSpooler: PBoolean; LocalCallBack,
        OnlineCallBack, SpoolerCallBack: TProc<Boolean>);
begin
  inherited Create(True);

  FLocalSvcChk:= aLocalSvc;
  FOnlineSvcChk:= aOnlineSvc;
  FSpoolerChk:= aSpooler;
  FLocalCallback := LocalCallBack;
  FOnlineCallback := OnlineCallBack;
  FSpoolerCallback := SpoolerCallBack;
end;

procedure TServiceChecker.Execute;
begin
  while not Terminated do
    begin
      if Assigned(FLocalSvcChk) then
        begin
          // Verifica o status da variável booleana
          Synchronize(
            procedure
            begin
              // Chama o callback com o resultado da verificação
              if Assigned(FLocalChecker) and Assigned(FLocalCallback) then
                begin
                  FLocalChecker;
                  FLocalCallback(FLocalSvcChk^);
                end;
            end
          );
        end;

      if Assigned(FOnlineSvcChk) then
        begin
          // Verifica o status da variável booleana
          Synchronize(
            procedure
            begin
              // Chama o callback com o resultado da verificação
              if Assigned(FOnlineChecker) and Assigned(FOnlineCallback) then
                begin
                  FOnlineChecker;
                  FOnlineCallback(FOnlineSvcChk^);
                end;
            end
          );
        end;

      if Assigned(FSpoolerChk) then
        begin
          // Verifica o status da variável booleana
          Synchronize(
            procedure
            begin
              // Chama o callback com o resultado da verificação
              if Assigned(FSpoolerChecker) and Assigned(FSpoolerCallback) then
                begin
                  FSpoolerChecker;
                  FSpoolerCallback(FSpoolerChk^);
                end;
            end
          );
        end;
      Sleep(30000);
    end;
end;


end.
