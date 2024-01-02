program Iso2Dxf;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Types,
  System.Classes,
  MBSoft.System,
  Iso2Dxf.Dxf in 'Iso2Dxf.Dxf.pas',
  Iso2Dxf.Iso in 'Iso2Dxf.Iso.pas';

var
  IsoLine, W: String;
  Words: TStringList;
  IsoBlock: TIsoBlock;
  I: Integer;

begin
  try
    IsoBlock:=TIsoBlock.Create();
    try
      //Prova lettura parametri della riga di comando
      if TMBCmdLine.HasParams then
      begin
        WriteLn('Parametri riga di comando: ',TMBCmdLine.Count);
        for I:=1 to TMBCmdLine.Count do
          WriteLn('Parametro ',I,': ',TMBCmdLine.Param[I])
      end;
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
