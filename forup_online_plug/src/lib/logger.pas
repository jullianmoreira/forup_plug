unit logger;

interface
uses System.Classes, System.StrUtils, System.IOUtils, System.Math, System.SysUtils,
System.DateUtils, System.JSON, System.TypInfo, System.Rtti, Generics.Collections, System.SyncObjs,
forup_types;

const
  LOG_LINE = '%s -> %s [%s] (%s)';

type
  {$M+}
  Tlogger = class(TObject)
  private
    FLogMessage: String;
    FLogDate: TDateTime;
    FLogID: String;
    FLogAdditionaInfo: TJSONValue;

    FLock: TCriticalSection;
  public
    procedure writeLog;
    constructor Create;
    destructor Destroy; override;
  published
    property LogMessage: String read FLogMessage write FLogMessage;
    property LogDate: TDateTime read FLogDate write FLogDate;
    property LogID: String read FLogID write FLogID;
    property LogAdditionaInfo: TJSONValue read FLogAdditionaInfo write FLogAdditionaInfo;
  end;

implementation
uses helpers;
{ Tlogger }

constructor Tlogger.Create;
begin
  inherited Create;

  FLock := TCriticalSection.Create;
end;

destructor Tlogger.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure Tlogger.writeLog;
var
  fHelper : Tfunc_helper;
  fileLog: TextFile;
  strMessage : String;
begin
  FLock.Enter;
  try
    try
      fHelper := Tfunc_helper.Create;
      // Abre o arquivo de log em modo de escrita (append)
      AssignFile(fileLog, fHelper.AppPath+LOGFILE);
      {$I-}
      Append(fileLog);
      {$I+}
      if IOResult <> 0 then
        begin
          // Se o arquivo não existir, cria um novo
          Rewrite(fileLog);
        end;

      strMessage := Format(LOG_LINE,[
        FormatDateTime('yyyy-mm-dd', Self.FLogDate),
        IfThen(Self.FLogID.IsEmpty, '0000', Self.FLogID),
        Self.FLogMessage,
        Ifthen(Self.FLogAdditionaInfo <> nil, Self.FLogAdditionaInfo.ToJSON, '{}')
      ]);
      // Escreve a mensagem no arquivo de log
      Writeln(fileLog, strMessage);

      // Fecha o arquivo
      CloseFile(fileLog);
    except
      on e : Exception do
        begin

        end;
    end;
  finally
    FreeAndNil(fHelper);
    // Sai da seção crítica
    FLock.Leave;
  end;
end;

end.
