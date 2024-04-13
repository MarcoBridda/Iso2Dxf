program Test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.Types,
  System.SysUtils,
  System.Math.Vectors,
  MBSoft.System.Math.Vectors;

var
  P1,P2: TPolygon;
  List: TPolygonList;

begin
  try
    P1.Add(PointF(10,10));
    P1.Add(PointF(10,20));
    P1.Add(PointF(20,20));

    P2:=Copy(P1,Low(P1),Length(P1));
    P2.Close;

    List:=TPolygonList.Create;
    try
      List.Add(P1);
      List.Add(P2);
    finally
      List.Free
    end;

    WriteLn;

    ReadLn
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
