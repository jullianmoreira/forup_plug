unit logger;

interface
uses System.Classes, System.StrUtils, System.IOUtils, System.SysUtils, System.Math,
System.DateUtils, System.JSON, System.TypInfo, System.Rtti, Generics.Collections;

type
  {$M+}
  Tlogger = class(TObject)
  private
    FLogMessage: String;
    FLogDate: String;
    FLogID: String;
    FLogAdditionaInfo: TJSONValue;
  published
    property LogMessage: String read FLogMessage write FLogMessage;
    property LogDate: String read FLogDate write FLogDate;
    property LogID: String read FLogID write FLogID;
    property LogAdditionaInfo: TJSONValue read FLogAdditionaInfo write FLogAdditionaInfo;
  end;

implementation

end.
