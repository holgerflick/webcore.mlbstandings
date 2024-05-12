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
  , System.SysUtils
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
        LStandingsItem.PCT := String( LTeamRecord['winPct'] );
        LStandingsItem.GB := String( LTeamRecord['gamesBack'] );
        LStandingsItem.WCGB := String( LTeamRecord['wildCardGamesBack'] );
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
  function SignedString(AInt: Integer): String;
  begin
    if AInt > 0 then
    begin
      Result := '+';
    end;

    Result := Result + IntToStr( AInt );
  end;

var
  LRoot: TJSElement;
  LTable: TJSElement;
  LCaption: TJSElement;

  LHeader: TJSElement;

  LBody,
  LRow: TJSElement;

  LItem: TStandingsItem;

begin
  await(FetchData);

  LRoot := document.getElementById(AElementName);
  if not Assigned(LRoot) then
  begin
    Exit;
  end;

  LTable := document.createElement('table');
  LCaption := document.createElement('caption');
  LCaption.innerText := 'Major League Baseball Standings';

  LHeader := document.createElement('thead');
  LHeader.innerHTML :=
  '''
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
  ''';

  LBody := document.createElement('tbody');

  for LItem in FStandings do
  begin
    LRow := document.createElement('tr');
    LRow.innerHTML :=
    Format(
    '''
      <td data-label="team">%s</td>
      <td data-label="wins">%d</td>
      <td data-label="losses">%d</td>
      <td data-label="pct">%s</td>
      <td data-label="games back">%s</td>
      <td data-label="wildcard behind">%s</td>
      <td data-label="streak">%s</td>
      <td data-label="runs scored">%d</td>
      <td data-label="runs against">%d</td>
      <td data-label="difference">%s</td>
      <td data-label="home">%s</td>
      <td data-label="away">%s</td>
    ''',
    [ LItem.Team, LItem.W, LItem.L, LItem.PCT, LItem.GB, LItem.WCGB,
      LItem.STRK, LItem.RS, LItem.RA, SignedString(LItem.DIFF), LItem.HOME, LItem.AWAY
    ] );

    LBody.appendChild(LRow);
  end;

  LTable.appendChild(LCaption);
  LTable.appendChild(LHeader);
  LTable.appendChild(LBody);
  LRoot.appendChild(LTable);
end;

end.
