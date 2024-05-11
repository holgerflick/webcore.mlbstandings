unit UResponsiveStandings;

interface
uses
  System.Generics.Collections
  ;

type
  TStandingsItem = class
  strict private
    FW: Integer;
    FL: Integer;
    FPCT: Double;
    FGB: Double;
    FWCGB: Double;
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
    property PCT: Double read FPCT write FPCT;
    property GB: Double read FGB write FGB;
    property WCGB: Double read FWCGB write FWCGB;
    property STRK: String read FSTRK write FSTRK;
    property RS: Integer read FRS write FRS;
    property RA: Integer read FRA write FRA;
    property DIFF: Integer read FDIFF write FDIFF;
    property HOME: String read FHOME write FHOME;
    property AWAY: String read FAWAY write FAWAY;

  end;

  TStandings = TObjectList<TStandingsItem>;

type
  TResponsiveStandings = class
  private
    FStandings: TStandings;

    [async]
    procedure FetchData;

  public
    constructor Create;
    destructor Destroy; override;

    [async]
    procedure GenerateStandings( AElementName: String );
  end;


implementation
uses
  Web
  , JS
  , WEBLib.REST
  ;

 const
   URL_SERVICE = 'http://192.168.4.47:3001/standings?division.id=201';

{ TResponsiveStandings }

constructor TResponsiveStandings.Create;
begin
  inherited;

  FStandings := TStandings.Create;
end;

destructor TResponsiveStandings.Destroy;
begin
  FStandings.Free;

  inherited;
end;

procedure TResponsiveStandings.FetchData;
var
  LHTTP: TWebHttpRequest;
  LResponse: TJSXMLHttpRequest;
  LArr: TJSArray;
  LStandings: TJSObject;
  LTeamRecords: TJSArray;
  LTeamRecord: TJSObject;
  i: Integer;

  LStandingsItem: TStandingsItem;

begin
  LHTTP := TWebHttpRequest.Create(nil);
  try
    LHttp.ResponseType := rtJSON;
    LHttp.Url := URL_SERVICE;
    LResponse := await( TJSXMLHttpRequest, LHttp.Perform );

    if LResponse.Status = 200 then
    begin
      FStandings.Clear;

      LArr :=  TJSArray( LResponse.response );
      LStandings := TJSObject( LArr[0] );
      LTeamRecords := TJSArray( LStandings['teamRecords'] );

      for i := 0 to LTeamRecords.Length - 1 do
      begin
        LTeamRecord := TJSObject( LTeamRecords[i] );

        LStandingsItem := TStandingsItem.Create;
        LStandingsItem.Team := String( LTeamRecord['teamName'] );
        LStandingsItem.W := Integer( LTeamRecord['wins'] );
        LStandingsItem.L := Integer( LTeamRecord['losses'] );
        LStandingsItem.PCT := Double( LTeamRecord['winPct'] );
        LStandingsItem.GB := Double( LTeamRecord['gamesBack'] );
        LStandingsItem.WCGB := Double( LTeamRecord['wildCardGamesBack'] );
        LStandingsItem.STRK := String( LTeamRecord['streak'] );
        LStandingsItem.RS := Integer( LTeamRecord['runsScored'] );
        LStandingsItem.RA := Integer( LTeamRecord['runsAllowed'] );
        LStandingsItem.DIFF := Integer( LTeamRecord['runDifferential'] );
        LStandingsItem.HOME := String( LTeamRecord['home'] );
        LStandingsItem.AWAY := String( LTeamRecord['away'] );

        FStandings.Add(LStandingsItem);
      end;

    end;



  finally
    LHTTP.Free;
  end;
end;

procedure TResponsiveStandings.GenerateStandings(AElementName: String);
var
  LRoot: TJSElement;
  LTable: TJSElement;

  LHeader: TJSElement;

  LBody,
  LRow,
  LCell: TJSElement;

begin
  LRoot := document.getElementById(AElementName);
  if not Assigned(LRoot) then
  begin
    Exit;
  end;

  LTable := document.createElement('table');
  LHeader := document.createElement('tr');
  LHeader.innerHTML :=
  '''
        <caption>Major League Baseball Standings</caption>
        <thead>
          <tr>
            <th>AL EAST</th>
            <th>W</th>
            <th>L</th>
            <th>PCT</th>
            <th>GB</th>
            <th>WCGB</th>
            <th>STRK</th>
            <th>RS</th>
            <th>RA</th>
            <th>DIFF</th>
            <th>HOME</th>
            <th>AWAY</th>
          </tr>
        </thead>
        <tbody>
        </tbody>
  ''';
  LTable.appendChild(LHeader);
  LRoot.appendChild(LTable);
end;

end.
