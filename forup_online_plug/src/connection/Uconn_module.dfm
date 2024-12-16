object conn_module: Tconn_module
  OnCreate = DataModuleCreate
  Height = 620
  Width = 1100
  PixelsPerInch = 120
  object fup_manager: TFDManager
    WaitCursor = gcrDefault
    FormatOptions.AssignedValues = [fvMapRules]
    FormatOptions.OwnMapRules = True
    FormatOptions.MapRules = <>
    Active = True
    Left = 80
    Top = 40
  end
  object fup_postgre: TFDConnection
    Left = 80
    Top = 120
  end
  object fup_mongo: TUniConnection
    DataTypeMap = <
      item
        DBType = 1607
        FieldType = ftWideString
      end
      item
        DBType = 1604
        FieldType = ftWideString
      end
      item
        DBType = 1605
        FieldType = ftWideString
      end
      item
        DBType = 1603
        FieldType = ftShortint
      end
      item
        DBType = 1612
        FieldType = ftWideString
      end>
    ProviderName = 'MongoDB'
    Port = 27017
    SpecificOptions.Strings = (
      'MongoDB.ConnectionOptions=ssl=false&writePreference=secondary'
      'MongoDB.LowerCaseObjectId=True'
      'MongoDB.UseUnicode=True')
    Username = 'forupdev08'
    Server = 'db1.wl10.aprendaerp.com.br'
    LoginPrompt = False
    Left = 180
    Top = 120
    EncryptedPassword = 'B9FF90FF8DFFAAFFC6FF9BFFCCFF89FFD5FF'
  end
  object fup_mongo_provider: TMongoDBUniProvider
    Left = 340
    Top = 70
  end
  object pg_driver: TFDPhysPgDriverLink
    Left = 530
    Top = 40
  end
  object fup_sqlite: TFDConnection
    Left = 80
    Top = 200
  end
  object fdWait: TFDGUIxWaitCursor
    Provider = 'Console'
    ScreenCursor = gcrDefault
    Left = 530
    Top = 120
  end
  object mysql_driver: TFDPhysMySQLDriverLink
    Left = 530
    Top = 200
  end
  object mssql_driver: TFDPhysMSSQLDriverLink
    Left = 530
    Top = 280
  end
  object pgQryList: TFDQuery
    Connection = fup_postgre
    FetchOptions.AssignedValues = [evMode, evAutoFetchAll]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvParamCreate, rvParamExpand]
    ResourceOptions.ParamCreate = False
    ResourceOptions.ParamExpand = False
    Left = 80
    Top = 300
  end
  object mongoQryList: TUniQuery
    Connection = fup_mongo
    Left = 350
    Top = 200
  end
  object pgCmd: TFDCommand
    Connection = fup_postgre
    Left = 80
    Top = 380
  end
  object mongoUpdQry: TUniQuery
    Connection = fup_mongo
    Left = 350
    Top = 280
  end
end
