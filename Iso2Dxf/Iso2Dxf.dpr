program Iso2Dxf;
//****************************************************************************
//Utility a riga di comando per trasformare un percorso in linguaggio ISO cnc
//in un DXF.
//Il file DXF viene generato nello stesso percorso del file ISO.
//
//Versione 1.1.0.0
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
  CurrentCncPosition: TPoint3D;
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
  if not ErrorMsg.IsEmpty then
  begin
    WriteLn(ErrorMsg);
    WriteLn;
  end;
  WriteLn('Sintassi: ',TAppInfo.AppName.ToUpper(),' isofile.cnc');
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
        CurrentCncPosition:=TPoint3D.Zero;
        IsMilling:=false;

        //Un po' di info
        WriteLn;
        WriteLn('File Iso: ', FileName.IsoFileName);
        WriteLn('File Dxf: ', FileName.DxfFileName);

        //Elaborazione
        for IsoBlock in CncFile do
        begin
          if not IsoBlock.IsEmpty then  //Elabora solo se c'è qualcosa
          begin
            for W in IsoBlock.Words do
            begin
              //Prima capiamo se stiamo lavorando (G1)
              if (W='G1') and not IsMilling then
              begin
                //Inserisci il punto di partenza della polilinea
                Polyline.Add(CurrentCncPosition.ToPointF);

                //Imposta il flag
                IsMilling:=true
              end;

              //oppure ci muoviamo in rapido (G0)
              if (W='G0') and IsMilling then
              begin
                //Inserisci la polilinea che hai trovato
                DxfFile.AddPolyline(Polyline);

                //Svuota la polilinea
                Polyline.Clear;

                //Resetta il flag
                IsMilling:=false
              end;

              //Poi aggiorniamo le posizioni degli assi x, y, z ad ogni blocco
              case W.Address of
                'X': CurrentCncPosition.X:=W.FloatValue;
                'Y': CurrentCncPosition.Y:=W.FloatValue;
                'Z': CurrentCncPosition.Z:=W.FloatValue
              end;

            end;
            //Aggiungiamo i punti solo se stiamo lavorando
            if IsMilling then
              Polyline.Add(CurrentCncPosition.ToPointF);
          end;
        end;

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
