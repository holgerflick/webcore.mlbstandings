unit UFrmMain;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, WEBLib.REST;

type
  TForm1 = class(TWebForm)
  [async]
    procedure WebFormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  UResponsiveStandings;

{$R *.dfm}

procedure TForm1.WebFormCreate(Sender: TObject);
var
  LStandings: TResponsiveStandings;

begin
  LStandings := TResponsiveStandings.Create;
  try
    await(LStandings.GenerateStandings('table-container'));
  finally
    LStandings.Free;
  end;
end;

end.