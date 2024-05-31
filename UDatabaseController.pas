unit UDatabaseController;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Modules, Data.DB,
  WEBLib.StellarDataStoreCDS, WEBLib.REST, WEBLib.StellarDataStore;

type
  TStandingsItem = class
  strict private
    FW: Integer;
    FL: Integer;
    FPCT: String;
    FGB: String;
    FWCGB: String;
    FSTRK: String;
    FRS: Integer;
    FRA: Integer;
    FDIFF: Integer;
    FHOME: String;
    FAWAY: String;

    FTeam: String;

  published
    property Team: String read FTeam write FTeam;

    property W: Integer read FW write FW;
    property L: Integer read FL write FL;
    property PCT: String read FPCT write FPCT;
    property GB: String read FGB write FGB;
    property WCGB: String read FWCGB write FWCGB;
    property STRK: String read FSTRK write FSTRK;
    property RS: Integer read FRS write FRS;
    property RA: Integer read FRA write FRA;
    property DIFF: Integer read FDIFF write FDIFF;
    property HOME: String read FHOME write FHOME;
    property AWAY: String read FAWAY write FAWAY;

  end;

type
  TDatabaseController = class(TWebDataModule)
    Standings: TWebStellarDataStoreClientDataset;
    Standingsid: TIntegerField;
    StandingsleagueId: TIntegerField;
    StandingsleagueName: TStringField;
    StandingsdivisionId: TIntegerField;
    StandingsdivisionName: TStringField;
    StandingsteamName: TStringField;
    Standingswins: TIntegerField;
    Standingslosses: TIntegerField;
    StandingswinPct: TStringField;
    StandingsgamesBack: TStringField;
    StandingswildCardGamesBack: TStringField;
    Standingsstreak: TStringField;
    StandingsrunsAllowed: TIntegerField;
    StandingsrunsScored: TIntegerField;
    StandingsrunDifferential: TIntegerField;
    Standingshome: TStringField;
    Standingsaway: TStringField;

    procedure WebDataModuleCreate(Sender: TObject);
    procedure WebDataModuleDestroy(Sender: TObject);
    procedure StandingsAfterOpen(DataSet: TDataSet);
  private
    FOnReady: TNotifyEvent;
    { Private declarations }


    procedure TransferToItem( AItem: TStandingsItem );
  public
    { Public declarations }
    procedure Open;
    procedure First;
    function Next( AItem: TStandingsItem ): Boolean;

    property OnReady: TNotifyEvent read FOnReady write FOnReady;
  end;


implementation

const
  ACCESS_TOKEN = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJodHRwOi8vc2NoZW1hcy5taWNyb3NvZnQuY29tL3dzLzIwMDgvMDYvaWRlbnRpdHkvY2xhaW1zL3JvbGUiOiJhY2Nlc3MtdG9rZW4iLCJodHRwOi8vc2NoZW1hcy54bWxzb2FwLm9yZy93cy8yMDA1LzA1L2lkZW50aXR5L2NsYWltcy9uYW1laWRlbnRpZmllci10b2tlbiI6ImY4YzRiMWRlLTQ4MTEtNGQxZC0zNTZhLTA4ZGMzZTMwOTE3MyIsImh0dHA6Ly9zY2hlbWFzLnhtbHNvYXAub3JnL3dzLzIwMDUvMDUvaWRlbnRpdHkvY2xhaW1zL25hbWVpZGVudGlmaWVyLXByb2plY3QiOiIxOGIxMWE4OS0xNTcwLTQ4Y2MtZTA3Yy0wOGRjMzc3OTg4YzAiLCJleHAiOjE3MTk0OTkxNDIsImlzcyI6Imh0dHBzOi8vYXBpLnN0ZWxsYXJkcy5pbyIsImF1ZCI6Imh0dHBzOi8vYXBpLnN0ZWxsYXJkcy5pbyJ9.ecCOgofXOybb30ev4ptjyep5yqBkNsByZnII8Zhlars';

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TDatabaseController.First;
begin
  Standings.First;
end;

function TDatabaseController.Next(AItem: TStandingsItem): Boolean;
begin
  Result := Standings.Eof;

  if not Result then
  begin
    TransferToItem( AItem );
  end;
end;

procedure TDatabaseController.Open;
begin
  console.log('Opening....');
  Standings.Open;
end;

procedure TDatabaseController.StandingsAfterOpen(DataSet: TDataSet);
begin
  console.log('AfterOpen fired.');
  if Assigned( FOnReady ) then
  begin
    FOnReady( DataSet );
  end;
end;

procedure TDatabaseController.TransferToItem(AItem: TStandingsItem);
begin
  AItem.Team := StandingsTeamName.AsString;
  AItem.W := StandingsWins.AsInteger;
  AItem.L := StandingsLosses.AsInteger;
  AItem.PCT := StandingsWinPct.AsString;
  AItem.GB := StandingsgamesBack.AsString;
  AItem.WCGB := StandingsWildCardGamesBack.AsString;
  AItem.STRK := StandingsStreak.AsString;
  AItem.RS  := StandingsRunsScored.AsInteger;
  AItem.RA := StandingsRunsAllowed.AsInteger;
  AItem.DIFF := StandingsRunDifferential.AsInteger;
  AItem.HOME := StandingsHome.AsString;
  AItem.AWAY := StandingsAway.AsString;
end;

procedure TDatabaseController.WebDataModuleCreate(Sender: TObject);
begin
  Standings.AccessToken := ACCESS_TOKEN;
end;

procedure TDatabaseController.WebDataModuleDestroy(Sender: TObject);
begin
  Standings.Close;
end;

end.
