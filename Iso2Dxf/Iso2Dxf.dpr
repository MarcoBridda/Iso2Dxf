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
  System.SysUtils,
  System.Classes,
  System.Math.Vectors,
  Winapi.Windows,
  MBSoft.System,
  MBSoft.System.SysUtils,
  MBSoft.System.IOUtils,
  MBSoft.Winapi.Windows,
  Iso2Dxf.Dxf in 'Source\Iso2Dxf.Dxf.pas',
  Iso2Dxf.Iso in 'Source\Iso2Dxf.Iso.pas',
  Iso2Dxf.Utils in 'Source\Iso2Dxf.Utils.pas';

//Una classe di eccezioni personalizzata per il programma principale
type
  EIso2DxfMain = class(Exception);

const
  //Costanti dei messaggi di errore
  NO_PARAM_FOUND  = 'Indicare il percorso di un file CNC';
  TOO_MANY_PARAMS = 'Troppi parametri';
  INVALID_PATH    = 'Percorso non valido';

var
  CncFile: TStringList;
  IsoBlock: TIsoBlock;
  DxfFile: TDxfFile;
  Line: String;
  FileName: TFileName;
  W: TIsoWord;
  Point: TPoint3D;
  IsMilling: Boolean;
  Polyline: TPolygon;

//un help che stampa la sintassi di chiamata dell'utility con la possibilità di
//stampare anche un messaggio di errore personalizzato
procedure Help(const ErrorMsg: String = '');
var
  ExeName: TFileName;
  ExeInfo: TVSFixedFileInfo;
begin
  ExeName:=TMBCmdLine.Param[0];
  ExeInfo.Init(ExeName);

  WriteLn;
  Write(ExeName.Name,' Versione ',ExeInfo.GetFileVersion.ToString,' ');
  WriteLn(TCopyInfo.GetLabel(2024));

  {$IFDEF DEBUG}
    WriteLn('[DEBUG MODE]');
  {$ENDIF}

  WriteLn;
  WriteLn('Sintassi: ISO2DXF isofile.cnc');
  writeLn;
  writeLn(ErrorMsg)
end;

{  -- MAIN --  }
begin
  try
    //No parametri
    if not TMBCmdLine.HasParams then
      raise EIso2DxfMain.Create(NO_PARAM_FOUND);
    //troppi parametri
    if TMBCmdLine.Count>1 then
      raise EIso2DxfMain.Create(TOO_MANY_PARAMS);

    FileName:=TMBCmdLine.Param[1];

    //Percorso non valido
    if not Filename.Exists then
      raise EIso2DxfMain.Create(INVALID_PATH + ': ' + FileName);

    //Se tutto va bene proseguiamo
    CncFile:=TStringList.Create();
    try
      CncFile.LoadFromFile(FileName);
      DxfFile:=TDxfFile.Create;
      try
        //Parte iniziale del dxf
        DxfFile.BeginEntities();

        //Inizializzazione
        Point:=TPoint3D.Zero;
        IsMilling:=false;

        IsoBlock:=TIsoBlock.Create();
        try
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
          DxfFile.AddPolyline(Polyline);
          //Parte finale del dxf e salvataggio
          DxfFile.EndSection();
          DxfFile.EndOfFile();
          DxfFile.SaveToFile(FileName.ChangeExt('.dxf'))
        finally
          IsoBlock.Free
        end;
      finally
        DxfFile.Free
      end;
    finally
      CncFile.Free
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
