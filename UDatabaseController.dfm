object DatabaseController: TDatabaseController
  OnCreate = WebDataModuleCreate
  OnDestroy = WebDataModuleDestroy
  Height = 318
  Width = 394
  object Standings: TWebStellarDataStoreClientDataset
    TableName = 'leagues'
    TableId = 0
    AfterOpen = StandingsAfterOpen
    Left = 104
    Top = 80
    object Standingsid: TIntegerField
      FieldName = 'id'
    end
    object StandingsleagueId: TIntegerField
      FieldName = 'leagueId'
    end
    object StandingsleagueName: TStringField
      FieldName = 'leagueName'
      Size = 255
    end
    object StandingsdivisionId: TIntegerField
      FieldName = 'divisionId'
    end
    object StandingsdivisionName: TStringField
      FieldName = 'divisionName'
      Size = 255
    end
    object StandingsteamName: TStringField
      FieldName = 'teamName'
      Size = 255
    end
    object Standingswins: TIntegerField
      FieldName = 'wins'
    end
    object Standingslosses: TIntegerField
      FieldName = 'losses'
    end
    object StandingswinPct: TStringField
      FieldName = 'winPct'
      Size = 10
    end
    object StandingsgamesBack: TStringField
      FieldName = 'gamesBack'
      Size = 10
    end
    object StandingswildCardGamesBack: TStringField
      FieldName = 'wildCardGamesBack'
      Size = 10
    end
    object Standingsstreak: TStringField
      FieldName = 'streak'
      Size = 10
    end
    object StandingsrunsAllowed: TIntegerField
      FieldName = 'runsAllowed'
    end
    object StandingsrunsScored: TIntegerField
      FieldName = 'runsScored'
    end
    object StandingsrunDifferential: TIntegerField
      FieldName = 'runDifferential'
    end
    object Standingshome: TStringField
      FieldName = 'home'
      Size = 10
    end
    object Standingsaway: TStringField
      FieldName = 'away'
      Size = 10
    end
  end
end
