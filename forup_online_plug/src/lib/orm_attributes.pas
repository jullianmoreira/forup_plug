unit orm_attributes;

interface
uses System.Rtti, System.Classes, System.SysUtils, System.StrUtils, System.WideStrUtils,
System.DateUtils, System.IOUtils, System.Types, System.TypInfo, System.JSON,
Generics.Collections;

type
  Torm_db_type = (ormString, ormReal, ormDate, ormDateTime, ormTime, ormWideString,
                  ormBoolean, ormInteger, ormUUID, ormJSON);
  Torm_sequence_type = (NotInc, AutoInc, GuidInc);
  Torm_restriction = (NotNull, NoInsert, NoUpdate, NoValidate, Unique, Hidden);
  Torm_restrictions = set of Torm_restriction;

  Tschema = class(TCustomAttribute)

  end;

  Tcolumn = class(TCustomAttribute)

  end;

  Tconstraint = class(TCustomAttribute)

  end;

  Tpk = class(TCustomAttribute)

  end;

  Ttable = class(TCustomAttribute)

  end;

implementation

end.
