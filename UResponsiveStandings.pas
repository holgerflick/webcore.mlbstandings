unit UResponsiveStandings;

interface
uses
    System.Generics.Collections
  , UDatabaseController
  ;


type
  TResponsiveStandings = class
  private
    FDatabaseController: TDatabaseController;

  public
    constructor Create;
    destructor Destroy; override;

    [async]
    procedure GenerateStandings( AElementName: String );
  end;


implementation
uses
  Web
  , System.SysUtils
  ;


{ TResponsiveStandings }

constructor TResponsiveStandings.Create;
begin
  inherited;

  FDatabaseController := TDatabaseController.Create(nil);
end;

destructor TResponsiveStandings.Destroy;
begin
  FDatabaseController.Free;

  inherited;
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

  FDatabaseController.First;

  LItem := TStandingsItem.Create;
  while (FDatabaseController.Next(LItem)) do
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
