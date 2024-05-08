unit entity_base;

interface
uses System.Rtti, System.Classes, System.SysUtils, System.StrUtils, System.WideStrUtils,
System.DateUtils, System.IOUtils, System.Types, System.TypInfo, System.JSON,
Generics.Collections, forup_types;

type
  TIbase_entity = interface
    ['{626BE4F7-D7B4-41F2-B786-7EEF70EEEABA}']

    procedure ClearObject;
    procedure CopyTo(aObj : TObject);
    procedure CopyFrom(aObj : TObject);
  end;

  {$M+}
  Tbase_entity = class(TInterfacedObject, TIbase_entity)
    private
      FPK_Value: TArray<String>;
      FPK_Field: TArray<String>;
    public
      procedure ClearObject; virtual;
      procedure CopyTo(aObj : TObject); virtual;
      procedure CopyFrom(aObj : TObject); virtual;
      constructor Create;
    published
      property PK_Value: TArray<String> read FPK_Value write FPK_Value;
      property PK_Field: TArray<String> read FPK_Field write FPK_Field;
  end;

implementation

{ Tbase_entity }

procedure Tbase_entity.ClearObject;
begin

end;

procedure Tbase_entity.CopyFrom(aObj: TObject);
begin

end;

procedure Tbase_entity.CopyTo(aObj: TObject);
begin

end;

constructor Tbase_entity.Create;
begin
  inherited Create;
end;

end.
