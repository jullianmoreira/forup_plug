object conn_module: Tconn_module
  Height = 496
  Width = 880
  object fup_manager: TFDManager
    FormatOptions.AssignedValues = [fvMapRules]
    FormatOptions.OwnMapRules = True
    FormatOptions.MapRules = <>
    Active = True
    Left = 64
    Top = 32
  end
  object fup_postgre: TFDConnection
    Left = 64
    Top = 96
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
    Left = 144
    Top = 96
    EncryptedPassword = 'B9FF90FF8DFFAAFFC6FF9BFFCCFF89FFD5FF'
  end
  object fup_mongo_provider: TMongoDBUniProvider
    Left = 272
    Top = 56
  end
  object pg_driver: TFDPhysPgDriverLink
    Left = 272
    Top = 120
  end
  object fup_sqlite: TFDConnection
    Left = 64
    Top = 160
  end
end
