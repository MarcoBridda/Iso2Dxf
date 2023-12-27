program Iso2Dxf;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Types, System.Classes;

var
  Rect: TRect;
  Dxf: TStringList;

procedure AddItemToDXF(const aDxf: TStrings; const Code: Integer; const Value: String);
begin
  aDxf.Add('  ' + Code.ToString());
  aDxf.Add(Value);
end;

begin
  try
    //Creiamo un rettangolo da 30x20mm e lo salviamo come dxf
    Rect:=TRect.Create(-15,-10,15,10);

    //Stampiamo un po' di info...
    WriteLn('Width : ', Rect.Width);
    WriteLn('Height: ', Rect.Height);
    WriteLn('Origin: ', Rect.CenterPoint.X,':',Rect.CenterPoint.Y);

    //Creiamo il dxf e scriviamo il rettangolo
    Dxf:=TStringList.Create();
    try
      //Cose da fare...
    finally
      Dxf.Free
    end;

    //Fine
    ReadLn
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
