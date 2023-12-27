program Iso2Dxf;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Types;

var
  Rect: TRect;

begin
  try
    //Creiamo un rettangolo da 30x20mm e lo salviamo come dxf
    Rect:=TRect.Create(-15,10,15,-10);

    //Stampiamo un po' di info...
    WriteLn('Width : ', Rect.Width);
    WriteLn('Height: ', Rect.Height);
    WriteLn('Origin: ', Rect.CenterPoint.X,':',Rect.CenterPoint.Y);

    //Fine
    ReadLn
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
