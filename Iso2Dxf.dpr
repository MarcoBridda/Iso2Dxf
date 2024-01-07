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

type
  T3DFloatPoint = record
    X, Y, Z: Single;

    constructor Create(const aX, aY, aZ: Single);
    procedure MoveTo(const aX, aY, aZ: Single);
    procedure MoveRel(const rX, rY, rZ: Single);
  end;

var
  CncFile: TStringList;
  IsoBlock: TIsoBlock;
  DxfFile: TDxfFile;
  Line: String;
  FileName: String;
  W: TIsoWord;

{ T3DFloatPoint }

constructor T3DFloatPoint.Create(const aX, aY, aZ: Single);
begin
  self.MoveTo(aX,aY,aZ)
end;

procedure T3DFloatPoint.MoveRel(const rX, rY, rZ: Single);
begin
  X:=X+rX;
  Y:=Y+rY;
  Z:=Z+rZ
end;

procedure T3DFloatPoint.MoveTo(const aX, aY, aZ: Single);
begin
  X:=aX;
  Y:=aY;
  Z:=aZ
end;


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
            for Line in CncFile do
            begin
              IsoBlock.Block:=Line;
              if not IsoBlock.IsEmpty then  //Elabora solo se c'è qualcosa
              begin
                WriteLn(Line);
                WriteLn('{');
                for W in IsoBlock.Words do
                begin
                  WriteLn('  ',W,' - ', W.Address,' = ',W.Value);
                end;
                WriteLn('}');
                WriteLn;
              end;
            end;
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
