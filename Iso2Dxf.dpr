program Iso2Dxf;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Types,
  Iso2Dxf.DxfClass in 'Iso2Dxf.DxfClass.pas';

var
  IsoLine: String;

//Normalizza il blocco di comandi ISO: tutto in maiuscolo ed eliminiamo gli
//e le tabulazioni
function NormalizeIso(IsoBlock: String): String;
begin
  Result:=IsoBlock.ToUpper().Replace(' ','',[rfReplaceAll]).Replace(#9,'',[rfReplaceAll]);
end;

begin
  try
    repeat
      Write('>');
      ReadLn(IsoLine);
      IsoLine:=NormalizeIso(IsoLine);
      WriteLn('Hai scritto: ',IsoLine);
    until IsoLine='';
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
