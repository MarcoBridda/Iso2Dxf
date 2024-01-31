program Iso2Dxf;
//****************************************************************************
//Utility a riga di comando per trasformare un percorso in linguaggio ISO cnc
//in un DXF.
//Il file DXF viene generato nello stesso percorso del file ISO.
//
//Versione 1.0.0.0
//
//Copyright MBSoft(2024)
//****************************************************************************

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Classes, System.Math.Vectors,
  MBSoft.System, MBSoft.System.SysUtils,
  Iso2Dxf.Dxf, Iso2Dxf.Iso, Iso2Dxf.Utils;

var
  CncFile: TStringList;
  IsoBlock: TIsoBlock;
  DxfFile: TDxfFile;
  Line: String;
  FileName: String;
  W: TIsoWord;
  Point: TPoint3D;
  IsMilling: Boolean;
  Polyline: TPolygon;

//un help che stampa la sintassi di chiamata dell'utility con la possibilità di
//stampare anche un messaggio di errore personalizzato
procedure Help(const ErrorMsg: String = '');
begin
  WriteLn('Iso2Dxf Versione 1.0.0.0');
  WriteLn(TCopyInfo.GetLabel(2024));
  WriteLn;
  WriteLn('Sintassi: ISO2DXF isofile.cnc');
  writeLn;
  writeLn(ErrorMsg)
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
            //Parte iniziale del dxf
            DxfFile.BeginEntities();

            //Inizializzazione
            Point:=TPoint3D.Zero;
            IsMilling:=false;

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

                  //Aggiorniamo le posizioni degli assi x, y, z ad ogni blocco
                  case W.Address of
                    'X': Point.X:=W.FloatValue;
                    'Y': Point.Y:=W.FloatValue;
                    'Z': Point.Z:=W.FloatValue
                  end;

                end;
                WriteLn;
                WriteLn('  Posizione attuale: ', Point.ToString(W.GetFloatSettings));
                //Per il momento aggiungiamo tutti i punti senza considerare G0 e G1
                Polyline.Add(Point.ToPointF);
                WriteLn('}');
                WriteLn;
              end;
            end;
            //Inserisci la polilinea che hai trovato
            Dxffile.AddPolyline(Polyline);
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
  except
    on E: Exception do
    begin
      Help(E.Message);
    {$IFDEF DEBUG}
      Writeln(E.ClassName, ': ', E.Message);
    {$ENDIF}
    end;
  end;
  {$IFDEF DEBUG}
    ReadLn
  {$ENDIF}
end.
