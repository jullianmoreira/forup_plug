unit forup_types;

interface
uses System.IOUtils, System.StrUtils, System.WideStrUtils, System.Math,
System.DateUtils, System.Classes, System.SysUtils, Generics.Collections, orm_attributes;

const

  LOGDIR = 'log'+PathDelim;
  LOGFILE = 'plugservice.log';
  CONN_CFG_DIR = 'db'+PathDelim;
  CONN_CFG_FILE = 'cfg.ini';

  EMPTY_SPACE = ' ';
  

type
  Tactive_db = (adbPostgres, adbMySQL, adbMSSQL, adbFirebird, adbSQLite);
  Tcfg_type = (ctDataBase, ctResource, ctService, ctOther);
  Tjoin_type = (jtNONE, jtINNER, jtLEFT, jtRIGHT, jtLEFTOUTER, jtRIGHTOUTER);

  Tformat = record
    Field : String;
    Mask : String;
    isFormating : Torm_db_type;
  end;

  {$M+}
  Tcondition_field = class(TObject)
  private
    FField: String;
    FOperation: String;
    FValue: String;
    FNextCondition: String;
  public
    procedure SelfClear;
    constructor Create(aField : String = ''; aOperation : String = ''; aValue : String = '';
      aNextCondition : String = '');
  published
    property Field: String read FField write FField;
    property Operation: String read FOperation write FOperation;
    property Value: String read FValue write FValue;
    property NextCondition: String read FNextCondition write FNextCondition;
  end;

  Tjoin_criteria = class(TObject)
  private
    FJoinEntity: String;
    FConditions: TList<Tcondition_field>;
    FJoinType: Tjoin_type;
  public
    procedure SelfClear;
    constructor Create(aJoinType : Tjoin_type = jtNONE; aJoinEnity : String = '';
      aConditions : TArray<Tcondition_field> = []);
    destructor Destroy; override;
  published
    property JoinType: Tjoin_type read FJoinType write FJoinType;
    property JoinEntity: String read FJoinEntity write FJoinEntity;
    property Conditions: TList<Tcondition_field> read FConditions write FConditions;
  end;

  Tcriteria = class(TObject)
    private
      JoinClause : TList<Tjoin_criteria>;
      Conditions : TList<Tcondition_field>;
      FOrderBy: String;
      FGroupBy: String;
      FHaving: String;

      ConditionDelete : TArray<Integer>;
      JoinDelete : TArray<Integer>;
      ConditionOnly : TArray<Integer>;
      JoinOnly : TArray<Integer>;
      ConditionMatch : TArray<String>;
      JoinMatch : TArray<String>;

    public
      constructor Create(aConditions : TArray<Tcondition_field> = [];
        aJoinClause : TArray<Tjoin_criteria> = []; aOrderBy : String = '';
        aGroupBy : String = ''; aHaving : String = '');
      destructor Destroy; override;
      procedure addCondition(aCondition : Tcondition_field);
      procedure addJoinClause(aJoin : Tjoin_criteria);
      procedure delCondition(aCondition : Tcondition_field); overload;
      procedure delCondition(aConditionID : Integer); overload;
      procedure delJoinClause(aJoin : Tjoin_criteria); overload;
      procedure deljoinCaluse(aJoinID : Integer); overload;

      procedure addConditionDelete(aCondition : Tcondition_field);
      procedure addConditionOnly(aCondition : Tcondition_field);
      procedure addConditionMatch(aCondition : Tcondition_field);
      procedure addJoinDelete(aJoin : Tjoin_criteria);
      procedure addJoinOnly(aJoin : Tjoin_criteria);
      procedure addJoinMatch(aJoin : Tjoin_criteria);


      function buildCriteria(aWhere : Boolean = true; aOrder : Boolean = true;
        aGroup : Boolean = true; aHaving : Boolean = true)  : TStringList;
    published
      property OrderBy: String read FOrderBy write FOrderBy;
      property GroupBy: String read FGroupBy write FGroupBy;
      property Having: String read FHaving write FHaving;
  end;

implementation
uses helpers;

{ Tcondition_field }

constructor Tcondition_field.Create(aField, aOperation, aValue,
  aNextCondition: String);
begin
  inherited Create;
  Self.FField := aField;
  Self.FOperation := aOperation;
  Self.FValue := aValue;
  Self.FNextCondition := aNextCondition;
end;

procedure Tcondition_field.SelfClear;
begin
  Self.FField := EmptyStr;
  Self.FOperation := EmptyStr;
  Self.FValue := EmptyStr;
  Self.FNextCondition := EmptyStr;
end;

{ Tjoin_criteria }

constructor Tjoin_criteria.Create(aJoinType: Tjoin_type; aJoinEnity: String;
  aConditions: TArray<Tcondition_field>);
var
  iCondition : Integer;
begin
  inherited Create;

  Self.FJoinEntity := aJoinEnity;
  Self.FJoinType := aJoinType;
  Self.FConditions := TList<Tcondition_field>.Create;
  if Length(aConditions) > 0 then
    begin
      for iCondition := Low(aConditions) to High(aConditions) do
        Self.FConditions.Add(Tcondition_field.Create(
          aConditions[iCondition].Field,
          aConditions[iCondition].Operation,
          aConditions[iCondition].Value,
          aConditions[iCondition].NextCondition
        ));
    end;
end;

destructor Tjoin_criteria.Destroy;
var
  iClear: Integer;
begin
  for iClear := Self.FConditions.Count-1 downto 0 do
    FreeAndNil(Self.FConditions.Items[iClear]);

  Self.FConditions.Clear;

  inherited Destroy;
end;

procedure Tjoin_criteria.SelfClear;
begin
  Self.FJoinEntity := EmptyStr;
  Self.FConditions.Clear;
  Self.FJoinType := jtNONE;
end;

{ Tcriteria }

procedure Tcriteria.addCondition(aCondition: Tcondition_field);
begin
  Self.Conditions.Add(aCondition);
end;

procedure Tcriteria.addConditionDelete(aCondition: Tcondition_field);
var
  iCriteria: Integer;
begin
  for iCriteria := 0 to Self.Conditions.Count-1 do
    begin
      if Self.Conditions.Items[iCriteria].Field = aCondition.Field then
        begin
          SetLength(Self.ConditionDelete, Length(Self.ConditionDelete)+1);
          Self.ConditionDelete[High(Self.ConditionDelete)] := iCriteria;
          Break;
        end;
    end;
end;

procedure Tcriteria.addConditionMatch(aCondition: Tcondition_field);
var
  iCriteria: Integer;
begin
  for iCriteria := 0 to Self.Conditions.Count-1 do
    begin
      if Self.Conditions.Items[iCriteria].Field = aCondition.Field then
        begin
          SetLength(Self.ConditionMatch, Length(Self.ConditionMatch)+1);
          Self.ConditionMatch[High(Self.ConditionMatch)] := aCondition.Field;
          Break;
        end;
    end;
end;

procedure Tcriteria.addConditionOnly(aCondition: Tcondition_field);
var
  iCriteria: Integer;
begin
  for iCriteria := 0 to Self.Conditions.Count-1 do
    begin
      if Self.Conditions.Items[iCriteria].Field = aCondition.Field then
        begin
          SetLength(Self.ConditionOnly, Length(Self.ConditionOnly)+1);
          Self.ConditionOnly[High(Self.ConditionOnly)] := iCriteria;
          Break;
        end;
    end;
end;

procedure Tcriteria.addJoinClause(aJoin: Tjoin_criteria);
begin
  Self.JoinClause.Add(aJoin);
end;

procedure Tcriteria.addJoinDelete(aJoin: Tjoin_criteria);
var
  iCriteria: Integer;
begin
  for iCriteria := 0 to Self.JoinClause.Count-1 do
    begin
      if Self.JoinClause.Items[iCriteria].JoinEntity = aJoin.JoinEntity then
        begin
          SetLength(Self.JoinDelete, Length(Self.JoinDelete)+1);
          Self.JoinDelete[High(Self.JoinDelete)] := iCriteria;
          Break;
        end;
    end;
end;

procedure Tcriteria.addJoinMatch(aJoin: Tjoin_criteria);
var
  iCriteria: Integer;
begin
  for iCriteria := 0 to Self.JoinClause.Count-1 do
    begin
      if Self.JoinClause.Items[iCriteria].JoinEntity = aJoin.JoinEntity then
        begin
          SetLength(Self.JoinMatch, Length(Self.JoinMatch)+1);
          Self.JoinMatch[High(Self.JoinMatch)] := aJoin.JoinEntity;
          Break;
        end;
    end;
end;

procedure Tcriteria.addJoinOnly(aJoin: Tjoin_criteria);
var
  iCriteria: Integer;
begin
  for iCriteria := 0 to Self.JoinClause.Count-1 do
    begin
      if Self.JoinClause.Items[iCriteria].JoinEntity = aJoin.JoinEntity then
        begin
          SetLength(Self.JoinOnly, Length(Self.JoinOnly)+1);
          Self.JoinOnly[High(Self.JoinOnly)] := iCriteria;
          Break;
        end;
    end;
end;

function Tcriteria.buildCriteria(aWhere : Boolean = true; aOrder : Boolean = true;
        aGroup : Boolean = true; aHaving : Boolean = true)  : TStringList;
var
  aWhereClause, aJoinClause : String;
  iCriteria, iJoinClause: Integer;
  goMount, buildOnly : Boolean;
  helper : Tfunc_helper;
  buildString : String;
begin
  Result := TStringList.Create;

  aWhereClause := IfThen(aWhere, ' WHERE ', ' AND ');

  helper := Tfunc_helper.Create;

  buildOnly := Length(JoinOnly) > 0;

  for iCriteria := 0 to Self.JoinClause.Count-1 do
    begin
      goMount := true;
      if Length(JoinDelete) > 0 then
        begin
          if helper.item_in_array(iCriteria, JoinDelete) then
            begin
              goMount := false;
            end;
        end
      else if Length(JoinOnly) > 0 then
        begin
          if helper.item_in_array(iCriteria, JoinOnly) then
            begin
              goMount := false;
            end;
        end
      else if Length(JoinMatch) > 0 then
        begin
          if helper.item_in_array(Self.JoinClause.Items[iCriteria].JoinEntity, JoinMatch) then
            begin
              goMount := true;
            end;
        end;

      buildString := EmptyStr;
      if goMount or (goMount = false and buildOnly) then
        begin
          case Self.JoinClause.Items[iCriteria].JoinType of
            jtINNER : buildString := ' INNER JOIN ';
            jtLEFT : buildString := ' LEFT JOIN ';
            jtRIGHT : buildString := ' RIGTH JOIN ';
            jtLEFTOUTER : buildString := ' LEFT OUTER JOIN ';
            jtRIGHTOUTER : buildString := ' RIGTH OUTER JOIN ';
          end;
          buildString := buildString + Self.JoinClause.Items[iCriteria].JoinEntity;
          aJoinClause := ' ON ';
          for iJoinClause := 0 to Self.JoinClause.Items[iCriteria].Conditions.Count-1 do
            begin
              if Self.JoinClause.Items[iCriteria].Conditions[iJoinClause].Field <> EmptyStr then
                begin
                  buildString := buildString + aJoinClause +
                    Self.JoinClause.Items[iCriteria].Conditions[iJoinClause].Field + EMPTY_SPACE +
                    Self.JoinClause.Items[iCriteria].Conditions[iJoinClause].Operation + EMPTY_SPACE +
                    Self.JoinClause.Items[iCriteria].Conditions[iJoinClause].Value + EMPTY_SPACE;
                  if Self.JoinClause.Items[iCriteria].Conditions[iJoinClause].NextCondition <> EmptyStr then
                    aJoinClause := EMPTY_SPACE + Self.JoinClause.Items[iCriteria].Conditions[iJoinClause].NextCondition + EMPTY_SPACE
                  else
                    aJoinClause := EMPTY_SPACE + 'AND' + EMPTY_SPACE;

                end;
            end;
          Result.Add(buildString);
        end;
    end;

  buildOnly := Length(ConditionOnly) > 0;
  for iCriteria := 0 to Self.Conditions.Count-1 do
    begin
      goMount := true;
      if Length(ConditionDelete) > 0 then
        begin
          if helper.item_in_array(iCriteria, ConditionDelete) then
            begin
              goMount := false;
            end;
        end
      else if Length(ConditionOnly) > 0 then
        begin
          if helper.item_in_array(iCriteria, ConditionOnly) then
            begin
              goMount := false;
            end;
        end
      else if Length(ConditionMatch) > 0 then
        begin
          if helper.item_in_array(Self.Conditions.Items[iCriteria].Field, ConditionMatch) then
            begin
              goMount := true;
            end;
        end;

      buildString := EmptyStr;
      if goMount or (goMount = false and buildOnly) then
        begin
          if Self.Conditions.Items[iCriteria].Field <> EmptyStr then
            begin
              buildString := aWhereClause;
              buildString := buildString + Self.Conditions[iCriteria].Field + EMPTY_SPACE +
              Self.Conditions[iCriteria].Operation + EMPTY_SPACE +
              Self.Conditions[iCriteria].Value + EMPTY_SPACE;
              if Self.Conditions[iCriteria].NextCondition <> EmptyStr then
                aWhereClause := EMPTY_SPACE + Self.JoinClause.Items[iCriteria].Conditions[iCriteria].NextCondition + EMPTY_SPACE
              else
                aWhereClause := EMPTY_SPACE + 'AND' + EMPTY_SPACE;

              Result.Add(buildString);
            end;
        end;

      if (Self.FOrderBy <> EmptyStr) and aOrder then
        Result.Add('ORDER BY'+EMPTY_SPACE+Self.FOrderBy);

      if (Self.FGroupBy <> EmptyStr) and aGroup then
        Result.Add('GROUP BY'+EMPTY_SPACE+Self.FGroupBy);

      if (Self.FHaving <> EmptyStr) and aHaving then
        Result.Add('HAVING'+EMPTY_SPACE+Self.FHaving);
    end;

  FreeAndNil(helper);
end;

constructor Tcriteria.Create(aConditions : TArray<Tcondition_field> = [];
        aJoinClause : TArray<Tjoin_criteria> = []; aOrderBy : String = '';
        aGroupBy : String = ''; aHaving : String = '');
var
  iCriteria: Integer;
begin
  inherited Create;
  Self.FOrderBy := aOrderBy;
  Self.FGroupBy := aGroupBy;
  Self.FHaving := aHaving;
  Self.Conditions := TList<Tcondition_field>.Create;
  Self.JoinClause := TList<Tjoin_criteria>.Create;
  if Length(aConditions) >  0 then
    begin
      for iCriteria := Low(aConditions) to High(aConditions) do
        begin
          Self.Conditions.Add(Tcondition_field.Create(
            aConditions[iCriteria].Field,
            aConditions[iCriteria].Operation,
            aConditions[iCriteria].Value,
            aConditions[iCriteria].NextCondition,
          ))
        end;
    end;

  if Length(aJoinClause) > 0 then
    begin
      for iCriteria := Low(aJoinClause) to High(aJoinClause) do
        begin
          Self.JoinClause.Add(Tjoin_criteria.Create(
              aJoinClause[iCriteria].JoinType,
              aJoinClause[iCriteria].JoinEntity,
              aJoinClause[iCriteria].Conditions.ToArray
            )
          )
        end;
    end;

  ConditionDelete := [];
  JoinDelete  := [];
  ConditionOnly  := [];
  JoinOnly := [];
  ConditionMatch := [];
  JoinMatch := [];
end;

procedure Tcriteria.delCondition(aCondition: Tcondition_field);
var
  iCriteria: Integer;
begin
  for iCriteria := Self.Conditions.Count-1 downto 0 do
    begin
      if Self.Conditions.Items[iCriteria].Field = aCondition.Field then
        begin
          Self.Conditions.Delete(iCriteria);
        end;
    end;
end;

procedure Tcriteria.delCondition(aConditionID: Integer);
begin
  if aConditionID <= (Self.Conditions.Count-1) then
    Self.Conditions.Delete(aConditionID);
end;

procedure Tcriteria.deljoinCaluse(aJoinID: Integer);
begin
  if aJoinID <= (Self.JoinClause.Count-1) then
    Self.JoinClause.Delete(aJoinID);
end;

procedure Tcriteria.delJoinClause(aJoin: Tjoin_criteria);
var
  iCriteria: Integer;
begin
  for iCriteria := Self.JoinClause.Count-1 downto 0 do
    begin
      if Self.JoinClause.Items[iCriteria].FJoinEntity = aJoin.JoinEntity then
        begin
          Self.JoinClause.Delete(iCriteria);
        end;
    end;
end;

destructor Tcriteria.Destroy;
var
  iClear: Integer;
begin
  for iClear := Self.Conditions.Count-1 downto 0 do
    FreeAndNil(Self.Conditions.Items[iClear]);

  for iClear := Self.JoinClause.Count-1 downto 0 do
    FreeAndNil(Self.JoinClause.Items[iClear]);

  Self.Conditions.Clear;
  Self.JoinClause.Clear;

  inherited Destroy;
end;

end.
