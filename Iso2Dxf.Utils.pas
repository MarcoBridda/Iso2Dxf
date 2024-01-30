unit Iso2Dxf.Utils;
//****************************************************************************
//Unit di utilità generica
//
//****************************************************************************

interface

  uses
    System.SysUtils, System.Math.Vectors;

  type
  //Visto che in più punti si converte da stringa a Single e viceversa, e sempre
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

  TPoint3DHelper = record helper for TPoint3D
  private
    const DEFAULT_PATTERN = '(X:Y:Z)';
  public
    function ToString(const Format: TFloatSettings; const Pattern: String = DEFAULT_PATTERN): String;
  end;

//****************************************************************************
function GetFormattedPoint(const P: TPoint3D; const Format: TFloatSettings): String;

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

//****************************************************************************
function GetFormattedPoint(const P: TPoint3D; const Format: TFloatSettings): String;
begin
  //Prima definiamo un pattern
  Result:='(X:Y:Z)';

  //Ora sostituiamo i valoro formattati nel pattern
  Result:=Result.Replace('X',Format.FloatToStr(P.X),[]);
  Result:=Result.Replace('Y',Format.FloatToStr(P.Y),[]);
  Result:=Result.Replace('Z',Format.FloatToStr(P.Z),[]);
end;

end.
