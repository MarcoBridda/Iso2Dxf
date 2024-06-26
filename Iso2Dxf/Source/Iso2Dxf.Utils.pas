unit Iso2Dxf.Utils;
//****************************************************************************
//Unit di utilit� generica
//
//****************************************************************************

interface

  uses
    System.Types, System.SysUtils, System.Math.Vectors, Winapi.Windows,
    MBSoft.System, MBSoft.System.IOUtils, MBSoft.Winapi.Windows;

  type
  //Visto che in pi� punti si converte da stringa a Single e viceversa, e sempre
  //con le stesse impostazioni, questa struttura le raggruppa ed espone alcuni
  //metodi utili
  TFloatSettings = record
    Format: TFloatFormat;
    Precision: Integer;
    Digits: Integer;
    Settings: TFormatSettings;

    //Restituisce le impostazioni di default
    class function Default: TFloatSettings; static;

    function StrToFloat(const Value: String): Single;
    function FloatToStr(const Value: Single): String;

    function NormalizeFloatStr(Value: String): String;
  end;

  //Record helper che raggruppa metodi personalizzati utili a manipolare i punti in 3D
  TPoint3DHelper = record helper for TPoint3D
  private
    const DEFAULT_PATTERN = '(X; Y; Z)';
  public
    //restituisce una stringa con le coordinate dei punti formattate in modo personalizzato
    function ToString(const Pattern: String = DEFAULT_PATTERN): String; overload;
    function ToString(const Format: TFloatSettings; const Pattern: String = DEFAULT_PATTERN): String; overload;

    //Converte un TPoint3D in un TPointF (2D) eliminando la Z
    function ToPointF: TPointF;
  end;

  //Una struttura statica per ricavare le info dell'app
  TAppInfo = record
  private
    class function GetExeName: TFileName; static;
    class function GetAppName: String; static;
    class function GetExeInfo: TVSFixedFileInfo; static;
  public
    class property ExeName: TFileName read GetExeName;
    class property AppName: String read GetAppName;
    class property ExeInfo: TVSFixedFileInfo read GetExeInfo;
  end;

  //Una struttura per gestire le coppie di nomi dei file Iso e Dxf, dato che
  //sono collegate.
  TIso2DxfFileName = record
  private
    FIsoFileName: TFileName;
    FDxfFileName: TFileName;
    FAutoUpdate: Boolean;

    //Setter
    procedure SetAutoUpdate(const Value: Boolean);
    procedure SetDxfFileName(const Value: TFileName);
    procedure SetIsoFileName(const Value: TFileName);

    //Utilit�
    procedure UpdateDxf;
    procedure UpdateIso;
  public
    //Nel costruttore si inizia con il nome del file Iso
    constructor Create(const aIsoFileName: TFileName; const aAutoUpdate: Boolean = true);

    //I metodi setter delle propriet� convertono un nome nell'altro e viceversa
    //in base a quale propriet� viene assegnata e al campo FAutoUpdate
    property IsoFileName: TFileName read FIsoFileName write SetIsoFileName;
    property DxfFileName: TFileName read FDxfFileName write SetDxfFileName;
    property AutoUpdate: Boolean read FAutoUpdate write SetAutoUpdate;
  end;

implementation

{ TFloatSettings }

class function TFloatSettings.Default: TFloatSettings;
begin
  Result.Format:=ffFixed;
  Result.Precision:=5;
  Result.Digits:=3;
  Result.Settings:=TFormatSettings.Create;
  Result.Settings.DecimalSeparator:='.';
end;

function TFloatSettings.FloatToStr(const Value: Single): String;
begin
  Result:=Value.ToString(Format,Precision,Digits,Settings)
end;

function TFloatSettings.NormalizeFloatStr(Value: String): String;
var
  DecSep, Sep: Char;
begin
  DecSep:=Settings.DecimalSeparator;
  for Sep in ['.',','] do
    if Sep<>DecSep then
      Value:=StringReplace(Value,Sep,DecSep,[]);
  if Value.EndsWith(DecSep) then
    Value:=Value+'0';
  Result:=Value
end;

function TFloatSettings.StrToFloat(const Value: String): Single;
begin
  Result:=System.SysUtils.StrToFloat(Value,Settings)
end;

{ TPoint3DHelper }

function TPoint3DHelper.ToPointF: TPointF;
begin
  Result:=PointF(Self.X,Self.Y)
end;

function TPoint3DHelper.ToString(const Format: TFloatSettings;
  const Pattern: String): String;
begin
  //Per sicurezza trasformiamo comunque le lettere degli assi in maiuscolo
  Result:=Pattern.ToUpper();

  //Ora sostituiamo i valori formattati nel pattern
  Result:=Result.Replace('X',Format.FloatToStr(Self.X),[]);
  Result:=Result.Replace('Y',Format.FloatToStr(Self.Y),[]);
  Result:=Result.Replace('Z',Format.FloatToStr(Self.Z),[]);
end;

function TPoint3DHelper.ToString(const Pattern: String): String;
begin
  Result:=Self.ToString(TFloatSettings.Default, Pattern);
end;

{ TAppInfo }

class function TAppInfo.GetExeInfo: TVSFixedFileInfo;
begin
  Result.Init(TAppInfo.GetExeName)
end;

class function TAppInfo.GetExeName: TFileName;
begin
  Result:=TMBCmdLine.Argument[0]
end;

class function TAppInfo.GetAppName: String;
begin
  Result:=String(TAppInfo.GetExeName.Name).Remove(High(TAppInfo.GetExeName.Name)-4)
end;

{ TIso2DxfFileName }

constructor TIso2DxfFileName.Create(const aIsoFileName: TFileName;
  const aAutoUpdate: Boolean);
begin
  FAutoUpdate:=aAutoUpdate;
  IsoFileName:=aIsoFileName;
end;

procedure TIso2DxfFileName.SetAutoUpdate(const Value: Boolean);
begin
  if FAutoUpdate xor Value then
  begin
    FAutoUpdate:=Value;

    //Se devo sincronizzare, quello che comanda � il nome del file Iso
    UpdateDxf
  end;
end;

procedure TIso2DxfFileName.SetDxfFileName(const Value: TFileName);
begin
  FDxfFileName := Value;
  UpdateIso
end;

procedure TIso2DxfFileName.SetIsoFileName(const Value: TFileName);
begin
  FIsoFileName := Value;
  UpdateDxf
end;

procedure TIso2DxfFileName.UpdateDxf;
begin
  if FAutoUpdate then
    FDxfFileName:=FIsoFileName.ChangeExt('.dxf')
end;

procedure TIso2DxfFileName.UpdateIso;
begin
  if FAutoUpdate then
    FIsoFileName:=FDxfFileName.ChangeExt('.iso')
end;

end.
