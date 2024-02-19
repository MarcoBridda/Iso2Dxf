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
  CncFile: TIsofile;
  IsoBlock: TIsoBlock;
  DxfFile: TDxfFile;
  FileName: TIso2DxfFileName;
  W: TIsoWord;
  Point: TPoint3D;
  IsMilling: Boolean;
  Polyline: TPolygon;

//Un messaggio di presentazione da stampare all'avvio dell' applicazione
procedure Hello;
begin
  WriteLn;
  Write(TAppInfo.ExeName.Name,' Versione ',TAppInfo.ExeInfo.GetFileVersion.ToString,' ');
  WriteLn(TCopyInfo.GetLabel(2024));

  {$IFDEF DEBUG}
    WriteLn('[DEBUG MODE]');
  {$ENDIF}
end;

//un help che stampa la sintassi di chiamata dell'utility con la possibilità di
//stampare anche un messaggio di errore personalizzato
procedure Help(const ErrorMsg: String = '');
begin
  WriteLn;
  WriteLn('Sintassi: ',TAppInfo.AppName.ToUpper(),' isofile.cnc');
  writeLn;
  writeLn(ErrorMsg)
end;

{  -- MAIN --  }
begin
  //Intanto salutiamo...
  Hello();
  try
    //No parametri
    if not TMBCmdLine.HasParams then
      raise EIso2DxfMain.Create(NO_PARAM_FOUND);
    //troppi parametri
    if TMBCmdLine.Count>1 then
      raise EIso2DxfMain.Create(TOO_MANY_PARAMS);

    FileName:=TIso2DxfFileName.Create(TMBCmdLine.Param[1]);

    //Un po' di info per vedere se tutto funziona
    WriteLn;
    WriteLn('File Iso: ', FileName.IsoFileName);
    WriteLn('File Dxf: ', FileName.DxfFileName);

    //Percorso non valido
    if not Filename.IsoFileName.Exists then
      raise EIso2DxfMain.Create(INVALID_PATH + ': ' + FileName.IsoFileName);

    //Se tutto va bene proseguiamo
    CncFile:=TIsofile.Create();
    try
      CncFile.LoadFromFile(FileName.IsoFileName);
      DxfFile:=TDxfFile.Create;
      try
        //Parte iniziale del dxf
        DxfFile.BeginEntities();

        //Inizializzazione
        Point:=TPoint3D.Zero;
        IsMilling:=false;

        //Elaborazione
        for IsoBlock in CncFile do
        begin
          if not IsoBlock.IsEmpty then  //Elabora solo se c'è qualcosa
          begin
            for W in IsoBlock.Words do
            begin
              //Aggiorniamo le posizioni degli assi x, y, z ad ogni blocco
              case W.Address of
                'X': Point.X:=W.FloatValue;
                'Y': Point.Y:=W.FloatValue;
                'Z': Point.Z:=W.FloatValue
              end;

            end;
            //Per il momento aggiungiamo tutti i punti senza considerare G0 e G1
            Polyline.Add(Point.ToPointF);
          end;
        end;
        //Inserisci la polilinea che hai trovato
        DxfFile.AddPolyline(Polyline);
        //Parte finale del dxf e salvataggio
        DxfFile.EndSection();
        DxfFile.EndOfFile();
        DxfFile.SaveToFile(FileName.DxfFileName)
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
