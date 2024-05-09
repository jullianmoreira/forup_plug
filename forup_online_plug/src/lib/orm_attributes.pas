unit orm_attributes;

interface
uses System.Rtti, System.Classes, System.SysUtils, System.StrUtils, System.WideStrUtils,
System.DateUtils, System.IOUtils, System.Types, System.TypInfo, System.JSON,
Generics.Collections;

type
  Torm_use_at = (uaInsert, uaUpdate, uaSelect);
  Torm_use = set of Torm_use_at;
  Torm_db_type = (ormString, ormReal, ormDate, ormDateTime, ormTime, ormWideString,
                  ormBoolean, ormInteger, ormUUID, ormJSON);
  Torm_sequence_type = (NotInc, AutoInc, GuidInc);
  Torm_restriction = (NoRestriction, NotNull, NoInsert, NoUpdate, NoValidate, Unique, Hidden);
  Torm_restrictions = set of Torm_restriction;

  {$M+}
  Tschema = class(TCustomAttribute)
  private
    FName: String;
    constructor Create(aName : String);
  published
    property Name: String read FName write FName;
  end;

  Tordenation = class(TCustomAttribute)
  private
    FFields: TArray<String>;
  public
    constructor Create(aFields : TArray<String> = []);
  published
    property Fields: TArray<String> read FFields write FFields;
  end;

  Tcolumn = class(TCustomAttribute)
  private
    FName: String;
    FRestrictions: Torm_restrictions;
    FSize: Integer;
    FPrecision: Integer;
    FMask: String;
    FCType: Torm_db_type;
  public
    constructor Create(aName : String; aSize : Integer; aCType : Torm_db_type; aMask : String = ''); overload;
    constructor Create(aName : String; aSize : Integer; aCType : Torm_db_type;
      aRestriction : Torm_restrictions; aMask : String = ''); overload;
  published
    property Name: String read FName write FName;
    property Restrictions: Torm_restrictions read FRestrictions write FRestrictions;
    property Size: Integer read FSize write FSize;
    property Precision: Integer read FPrecision write FPrecision;
    property Mask: String read FMask write FMask;
    property CType: Torm_db_type read FCType write FCType;
  end;

  Tconstraint = class(TCustomAttribute)

  end;

  Tpk = class(TCustomAttribute)
  private
    FFields: TArray<Tcolumn>;
    FPkType: Torm_sequence_type;
  public
    constructor Create(aFields : TArray<Tcolumn>; aPKType : Torm_sequence_type);
  published
    property Fields: TArray<Tcolumn> read FFields write FFields;
    property PkType: Torm_sequence_type read FPkType write FPkType;
  end;

  Ttable = class(TCustomAttribute)
  private
    FName: String;
    FSchema: Tschema;
  public
    constructor Create(aName : String; aSchema : String = '');
  published
    property Name: String read FName write FName;
    property Schema: Tschema read FSchema write FSchema;
  end;

implementation

{ Tschema }

constructor Tschema.Create(aName: String);
begin
  inherited Create;
  Self.FName := aName;
end;

{ Ttable }

constructor Ttable.Create(aName : String; aSchema : String = '');
begin
  inherited Create;
  Self.Name := aName;
  Self.FSchema := Tschema.Create(aSchema);
end;

{ Tordenation }

constructor Tordenation.Create(aFields: TArray<String>);
var
  iField: Integer;
begin
  inherited Create;

  if Length(aFields) > 0 then
    begin
      SetLength(Self.FFields, Length(aFields));
      for iField := Low(aFields) to High(aFields) do
        begin
          Self.FFields[iField] := aFields[iField];
        end;
    end
  else SetLength(Self.FFields, 0);

end;

{ Tcolumn }

constructor Tcolumn.Create(aName : String; aSize : Integer; aCType : Torm_db_type; aMask : String = '');
begin
  inherited Create;

  Self.FName := aName;
  Self.FSize := aSize;
  Self.FPrecision := -1;
  Self.FCType := aCType;
  Self.Mask := aMask;
  Self.FRestrictions := [NoRestriction];
end;

constructor Tcolumn.Create(aName: String; aSize: Integer; aCType: Torm_db_type;
  aRestriction: Torm_restrictions; aMask: String);
begin
  Create(aName, aSize, aCType, aMask);
  Self.FRestrictions := aRestriction;
end;

{ Tpk }

constructor Tpk.Create(aFields: TArray<Tcolumn>; aPKType: Torm_sequence_type);
var
  iField: Integer;
begin
  inherited Create;
  SetLength(Self.FFields, Length(aFields));
  for iField := Low(aFields) to High(aFields) do
    begin
      Self.FFields[iField] := Tcolumn.Create(
        aFields[iField].Name,
        aFields[iField].Size,
        aFields[iField].CType
      );
    end;
end;

end.
