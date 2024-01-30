unit Iso2Dxf.Dxf;
//Una una unit per la parte dxf

interface

uses
  System.Classes, System.Types, System.SysUtils, System.Math.Vectors,
  Iso2Dxf.Utils;

type
  TDxfFile = class
  private
    FLines: TstringList;
    FFloatSettings: TFloatSettings;

    //Le informazioni vanno a paio: un codice numerico e un valore associato
    //ma vanno scritte su due righe separate
    procedure Add(const Code: Integer; const Value: String);
  public
    constructor Create;
    destructor Destroy; override;

    //Metodi per aggiungere elementi specifici
    procedure BeginEntities();
    procedure BeginPolyline(const IsClosed: Boolean = false);
    procedure AddVertex(const X,Y: Integer);
    procedure EndPolyline();
    procedure EndSection();
    procedure EndOfFile();

    //Aggiunge un' intera polilinea prendendo i punti da un array TPolygon
    procedure AddPolyline(List : TPolygon);

    //Salva il Dxf sul disco
    procedure SaveToFile(const FileName: string);
  end;

implementation

{ TDxfClass }

procedure TDxfFile.Add(const Code: Integer; const Value: String);
begin
  FLines.Add('  ' + Code.ToString);
  FLines.Add(Value);
end;

procedure TDxfFile.AddPolyline(List: TPolygon);
var
  Vertex: TPointF;
begin
  if List.IsPolyline then
  begin
    BeginPolyline();

    //Aggiungi vertici

    EndPolyline();
  end;
end;

procedure TDxfFile.AddVertex(const X, Y: Integer);
begin
  Add(0,'VERTEX');
  Add(8,'0');   //Layer
  Add(10,X.ToString());
  Add(20,Y.ToString());
  Add(30,'0');
end;

procedure TDxfFile.BeginEntities;
begin
  Add(0,'SECTION');
  Add(2,'ENTITIES');
end;

procedure TDxfFile.BeginPolyline(const IsClosed: Boolean);
begin
  Add(0,'POLYLINE');
  Add(8,'0');     //Layer

  if IsClosed then
    Add(70,'1');  //Polilinea chiusa

  Add(10,'0');
  Add(20,'0');
  Add(30,'0');
end;

constructor TDxfFile.Create;
begin
  FLines:=TStringList.Create;

  //Inizializza alle impostazioni di default
  FFloatSettings:=TFloatSettings.Default
end;

destructor TDxfFile.Destroy;
begin
  FLines.Free;

  inherited;
end;

procedure TDxfFile.EndOfFile;
begin
  Add(0,'EOF');
end;

procedure TDxfFile.EndPolyline;
begin
  Add(0,'SEQEND');
end;

procedure TDxfFile.EndSection;
begin
  Add(0,'ENDSEC');
end;

procedure TDxfFile.SaveToFile(const FileName: string);
begin
  FLines.SaveToFile(FileName);
end;

end.
