program Iso2Dxf;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Types,
  System.Classes,
  MBSoft.System,
  Iso2Dxf.Dxf in 'Iso2Dxf.Dxf.pas',
  Iso2Dxf.Iso in 'Iso2Dxf.Iso.pas',
  Iso2Dxf.Utils in 'Iso2Dxf.Utils.pas';

var
  CncFile: TStringList;
  IsoBlock: TIsoBlock;
  DxfFile: TDxfFile;
  Line: String;
  FileName: String;
  W: TIsoWord;
  Point: T3DFloatPoint;

{  -- MAIN --  }
begin
  try
    if TMBCmdLine.HasParams then
    begin
      IsoBlock:=TIsoBlock.Create();
      try
        CncFile:=TStringList.Create();
        try
          FileName:=TMBCmdline.Param[1];
          CncFile.LoadFromFile(FileName);
          DxfFile:=TDxfFile.Create;
          try
            //Parte iniziale del dxf
            DxfFile.BeginEntities();
            //Elaborazione
            for Line in CncFile do
            begin
              IsoBlock.Block:=Line;
              if not IsoBlock.IsEmpty then  //Elabora solo se c'è qualcosa
              begin
                WriteLn(Line);
                WriteLn('{');
                for W in IsoBlock.Words do
                begin
                  WriteLn('  ',W,' - ', W.Address,' = ',W.StringValue);
                end;
                WriteLn('}');
                WriteLn;
              end;
            end;
            //Parte finale del dxf e salvataggio
            DxfFile.EndSection();
            dxfFile.EndOfFile();
            DxfFile.SaveToFile(ChangeFileExt(FileName,'.dxf'));
          finally
            DxfFile.Free;
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
