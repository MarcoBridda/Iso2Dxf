program Iso2Dxf;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Types,
  System.Classes,
  Iso2Dxf.Dxf in 'Iso2Dxf.Dxf.pas',
  Iso2Dxf.Iso in 'Iso2Dxf.Iso.pas';

var
  IsoLine, W: String;
  Words: TStringList;
  IsoBlock: TIsoBlock;

begin
  try
    IsoBlock:=TIsoBlock.Create();
    try
      repeat
      Write('cnc>');
      ReadLn(IsoLine);
      if IsoLine<>'' then
      begin
        IsoBlock.Block:=IsoLine;

        WriteLn('Hai scritto: ',IsoLine);

        for W in IsoBlock.Words do
          WriteLn('  ',W);
      end;
    until IsoLine='';
    finally
      IsoBlock.Free
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
