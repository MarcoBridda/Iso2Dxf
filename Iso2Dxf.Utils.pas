unit Iso2Dxf.Utils;
//****************************************************************************
//Unit di utilit� generica
//
//****************************************************************************

interface

  uses
    System.Types, System.SysUtils, System.Math.Vectors;

  type
  //Visto che in pi� punti si converte da stringa a Single e viceversa, e sempre
  //con le stesse impostazioni, questa struttura le raggruppa ed espone alcuni
  //metodi utili
  TFloatSettings = record
    Format: TFloatFormat;
    Precision: Integer;
    Digits: Integer;
    Settings: TFormatSettings;

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
    function ToString(const Format: TFloatSettings; const Pattern: String = DEFAULT_PATTERN): String;

    //Converte un TPoint3D in un TPointF (2D) eliminando la Z
    function ToPointF: TPointF;
  end;

  //Un record helper per il tipo TPolygon per aggiungere e togliere punti
  TPolygonHelper = record helper for TPolygon

  end;

implementation

{ TFloatSettings }

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

end.
