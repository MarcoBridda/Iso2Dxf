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
      //Tanto per fare una prova lo scriviamo a manina...
      AddItemToDXF(Dxf,0,'SECTION');
      AddItemToDXF(Dxf,2,'ENTITIES');


      AddItemToDXF(Dxf,0,'ENDSEC');
      AddItemToDXF(Dxf,0,'EOF');

      //Scriviamo il file sul disco
      Dxf.SaveToFile('Prova.dxf');
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
