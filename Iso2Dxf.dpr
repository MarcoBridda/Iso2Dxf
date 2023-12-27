program Iso2Dxf;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Types,
  Iso2Dxf.DxfClass in 'Iso2Dxf.DxfClass.pas';

var
  Rect: TRect;
  Dxf: TDxfClass;

begin
  try
    //Creiamo un rettangolo da 30x20mm e lo salviamo come dxf
    Rect:=TRect.Create(-15,-10,15,10);

    //Stampiamo un po' di info...
    WriteLn('Width : ', Rect.Width);
    WriteLn('Height: ', Rect.Height);
    WriteLn('Origin: ', Rect.CenterPoint.X,':',Rect.CenterPoint.Y);

    //Creiamo il dxf e scriviamo il rettangolo
    Dxf:=TDxfClass.Create;
    try
      Dxf.BeginEntities();
      Dxf.BeginPolyline();
      Dxf.AddVertex(Rect.Left,Rect.Top);
      Dxf.AddVertex(Rect.Right,Rect.Top);
      Dxf.AddVertex(Rect.Right,Rect.Bottom);
      Dxf.AddVertex(Rect.Left,Rect.Bottom);
      Dxf.EndPolyline();
      Dxf.EndSection();
      Dxf.EndOfFile();

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
