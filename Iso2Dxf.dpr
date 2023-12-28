program Iso2Dxf;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Types,
  Iso2Dxf.DxfClass in 'Iso2Dxf.DxfClass.pas';

var
  IsoLine: String;

begin
  try
    repeat
      Write('>');
      ReadLn(IsoLine);
      WriteLn('Hai scritto: ',IsoLine);
    until IsoLine='';
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
