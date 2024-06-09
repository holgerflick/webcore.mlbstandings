unit UFrmMain;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, WEBLib.REST
  ,   UResponsiveStandings;

type
  TForm1 = class(TWebForm)

    procedure WebFormCreate(Sender: TObject);
    procedure WebFormDestroy(Sender: TObject);
  private
    FStandings: TResponsiveStandings;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

procedure TForm1.WebFormCreate(Sender: TObject);
begin
  FStandings := TResponsiveStandings.Create('table-container');
  FStandings.GenerateStandings;
end;

procedure TForm1.WebFormDestroy(Sender: TObject);
begin
  FStandings.Free;
end;

end.