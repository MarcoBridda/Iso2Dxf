program Iso2Dxf;
//****************************************************************************
//Utility a riga di comando per trasformare un percorso in linguaggio ISO cnc
//in un DXF.
//Il file DXF viene generato nello stesso percorso del file ISO.
//
//Versione 1.3.0.0
//
//Copyright MBSoft(2024)
//****************************************************************************

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  //System.Classes,
  System.Math.Vectors,
  Winapi.Windows,
  MBSoft.System,
  MBSoft.System.SysUtils,
  MBSoft.System.Math.Vectors,
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
  CncFile: TIsofile;             //Il file cnc
  PolyList: TPolygonList;        //La Lista di tutti i percorsi trovati
  DxfFile: TDxfFile;             //Il file dxf
  FileName: TIso2DxfFileName;    //Una struttura per gestire i nomi dei files
  CurrentCncPosition: TPoint3D;  //Posizione corrente degli assi X, Y, Z
  Polyline: TPolygon;            //Percorso cnc fatto in G1
  IsMilling: Boolean;            //Stiamo fresando (G1) oppure no (G0)?
  IsoHasError: Boolean;          //Ci sono errori di sintassi nel file cnc?

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

//Gestione della notifica errori di sintassi nel file iso
procedure MainOnIsoSyntaxError(Sender: TObject; E: Exception; Msg: String);
begin
  //Settiamo il flag
  IsoHasError:=true;

  //Se siamo in debug stampa anche l'eccezione
  {$IFDEF DEBUG}
    Write('<', E.ClassName, ': ', E.Message, '> ');
  {$ENDIF}

  WriteLn(Msg)
end;

//In caso di errori chiede se si vuole ugualmente convertire
function ConvertAnyway: Boolean;
const
  //Possibili risposte
  TRUE_ANSWER : TSysCharSet = ['s', 'S'];
  FALSE_ANSWER: TSysCharSet = ['n', 'N'];
var
  CaretPos: TCoord;
  Response: String;
begin
  WriteLn;
  WriteLn('Il file presenta degli errori di sintassi.');
  WriteLn('Le righe contenenti gli errori sono state ignorate e non verranno processate.');
  Write('Si vuole procedere comunque alla conversione in dxf? [s,n] ');

  CaretPos:=TConsole.CursorPosition;

  repeat
    TConsole.CursorPosition:=CaretPos;
    TConsole.ClrEOL;
    ReadLn(Response)
  until (Length(Response)=1) and CharInSet(Response[1], TRUE_ANSWER + FALSE_ANSWER);

  Result:=CharInSet(Response[1], TRUE_ANSWER)
end;

{  -- MAIN --  }
begin
  try
    //Intanto salutiamo...
    Hello();

    //No parametri
    if not TMBCmdLine.HasArguments then
      raise EIso2DxfMain.Create(NO_PARAM_FOUND);
    //troppi parametri
    if TMBCmdLine.Count>1 then
      raise EIso2DxfMain.Create(TOO_MANY_PARAMS);

    FileName:=TIso2DxfFileName.Create(TMBCmdLine.Argument[1]);

    //Percorso non valido
    if not Filename.IsoFileName.Exists then
      raise EIso2DxfMain.Create(INVALID_PATH + ': ' + FileName.IsoFileName);

    //Se tutto va bene proseguiamo
    CncFile:=TIsofile.Create();
    try
      //Inizializzazione
      CurrentCncPosition:=TPoint3D.Zero;
      IsMilling:=false;
      IsoHasError:=false;

      //Un po' di info
      WriteLn;
      WriteLn('File Iso: ', FileName.IsoFileName);
      WriteLn('File Dxf: ', FileName.DxfFileName);
      WriteLn;

      //Poi colleghiamo il gestore dell'evento OnSyntaxError
      CncFile.OnSyntaxError:=MainOnIsoSyntaxError;

      //Poi carichiamo e controlliamo
      CncFile.LoadFromFile(FileName.IsoFileName);

      if not IsoHasError or ConvertAnyway then
      begin
        PolyList:=TPolygonList.Create;
        try
          //Elaborazione
          for var IsoBlock: TIsoBlock in CncFile do
          begin
            if not IsoBlock.IsEmpty then  //Elabora solo se c'è qualcosa
            begin
              for var Word: TIsoWord in IsoBlock.Words do
              begin
                //Prima capiamo se stiamo lavorando (G1)
                if (Word='G1') and not IsMilling then
                begin
                  //Inserisci il punto di partenza della polilinea
                  Polyline.Add(CurrentCncPosition.ToPointF);

                  //Imposta il flag
                  IsMilling:=true
                end;

                //oppure ci muoviamo in rapido (G0)
                if (Word='G0') and IsMilling then
                begin
                  //Inserisci la polilinea che hai trovato
                  PolyList.Add(Polyline);

                  //Svuota la polilinea
                  Polyline.Clear;

                  //Resetta il flag
                  IsMilling:=false
                end;

                //Poi aggiorniamo le posizioni degli assi x, y, z ad ogni blocco
                case Word.Address of
                  'X': CurrentCncPosition.X:=Word.FloatValue;
                  'Y': CurrentCncPosition.Y:=Word.FloatValue;
                  'Z': CurrentCncPosition.Z:=Word.FloatValue
                end;

              end;
              //Aggiungiamo i punti solo se stiamo lavorando
              if IsMilling then
                Polyline.Add(CurrentCncPosition.ToPointF);
            end;
          end;

          //Finiti percorsi creiamo il file DXF
          DxfFile:=TDxfFile.Create;
          try
            //Parte iniziale del dxf
            DxfFile.BeginEntities();

            //Aggiungiamo i percorsi
            for var CncPath: TPolygon in PolyList do
              DxfFile.AddPolyline(CncPath);

            //Parte finale del dxf e salvataggio
            DxfFile.EndSection();
            DxfFile.EndOfFile();
            DxfFile.SaveToFile(FileName.DxfFileName)
          finally
            DxfFile.Free;
          end;

        finally
          //DxfFile.Free
          PolyList.Free
        end;
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
