program Iso2Dxf;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Types;

var
  Rect: TRect;

begin
  try
    //Creiamo un rettangolo e lo salviamo come dxf
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
