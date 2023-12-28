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
    

    //Fine
    ReadLn
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
