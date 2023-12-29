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
function NormalizeIso(const IsoBlock: String): String;
begin
  Result:=IsoBlock.ToUpper().Replace(' ','',[rfReplaceAll]).Replace(#9,'',[rfReplaceAll]);
end;

//Elimina i commenti dal blocco ISO
function SkipComment(const IsoBlock: String): String;
var
  PosB, PosE: Integer; //Inizio e fine dei commenti
begin
  Result:=IsoBlock;

  PosB:=Result.IndexOf('(');

  while PosB>-1 do
  begin
    PosE:=Result.IndexOf(')',PosB+1);

    if PosE=-1 then
      Result:=Result.Remove(PosB)
    else
      Result:=Result.Remove(PosB,PosE-PosB+1);

    PosB:=Result.IndexOf('(',PosE+1)
  end;
end;

begin
  try
    repeat
      Write('>');
      ReadLn(IsoLine);
      IsoLine:=SkipComment(IsoLine);
      IsoLine:=NormalizeIso(IsoLine);
      WriteLn('Hai scritto: ',IsoLine);
    until IsoLine='';
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
