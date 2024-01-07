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
  CncFile: TStringList;
  IsoBlock: TIsoBlock;
  Line: String;
  W: TIsoWord;

begin
  try
    if TMBCmdLine.HasParams then
    begin
      IsoBlock:=TIsoBlock.Create();
      try
        CncFile:=TStringList.Create();
        try
          CncFile.LoadFromFile(TMBCmdline.Param[1]);
          for Line in CncFile do
          begin
            IsoBlock.Block:=Line;
            if not IsoBlock.IsEmpty then  //Elabora solo se c'è qualcosa
            begin
              WriteLn(Line);
              WriteLn('{');
              for W in IsoBlock.Words do
                WriteLn('  ',W,' - ', W.Address,' = ',W.Value);
              WriteLn('}');
              WriteLn;
            end;
          end;
        finally
          CncFile.Free
        end;
      finally
        IsoBlock.Free
      end;
    end;
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
