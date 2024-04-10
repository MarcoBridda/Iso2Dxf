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

begin
  try
    P1.Add(PointF(10,10));
    P1.Add(PointF(10,20));
    P1.Add(PointF(20,20));

    WriteLn('Length  : ', Length(P1));
    WriteLn('IsOpened: ', P1.IsOpened);
    WriteLn('IsClosed: ', P1.IsClosed);

    P1.Close;
    WriteLn;

    WriteLn('Length  : ', Length(P1));
    WriteLn('IsOpened: ', P1.IsOpened);
    WriteLn('IsClosed: ', P1.IsClosed);

    P2:=Copy(P1,low(P1),Length(P1));
    WriteLn;

    WriteLn('IsEqual : ', P1.IsEqual(P2));

    P2[3]:=PointF(40,40);
    WriteLn;

    WriteLn('IsEqual : ', P1.IsEqual(P2));
    WriteLn('Length  : ', Length(P2));
    WriteLn('IsOpened: ', P2.IsOpened);
    WriteLn('IsClosed: ', P2.IsClosed);

    P2.Close;
    P2.Add(PointF(20,20));
    WriteLn;

    WriteLn('IsEqual : ', P1.IsEqual(P2));
    WriteLn('Length  : ', Length(P2));
    WriteLn('IsOpened: ', P2.IsOpened);
    WriteLn('IsClosed: ', P2.IsClosed);

    ReadLn
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
