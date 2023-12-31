unit Iso2Dxf.Dxf;
//Una classe per rappresentare un file dxf

interface

uses
  System.Classes, System.SysUtils;

type
  TDxfClass = class
  private
    FDxfFile: TstringList;

    //Le informazioni vanno a paio: un codice numerico e un valore associato
    //ma vanno scritte su due righe separate
    procedure AddItem(const Code: Integer; const Value: String);
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

    //Salva il Dxf sul disco
    procedure SaveToFile(const FileName: string);
  end;

implementation

{ TDxfClass }

procedure TDxfClass.AddItem(const Code: Integer; const Value: String);
begin
  FDxfFile.Add('  ' + Code.ToString);
  FDxfFile.Add(Value);
end;

procedure TDxfClass.AddVertex(const X, Y: Integer);
begin
  AddItem(0,'VERTEX');
  AddItem(8,'0');   //Layer
  AddItem(10,X.ToString());
  AddItem(20,Y.ToString());
  AddItem(30,'0');
end;

procedure TDxfClass.BeginEntities;
begin
  AddItem(0,'SECTION');
  AddItem(2,'ENTITIES');
end;

procedure TDxfClass.BeginPolyline(const IsClosed: Boolean);
begin
  AddItem(0,'POLYLINE');
  AddItem(8,'0');     //Layer

  if IsClosed then
    AddItem(70,'1');  //Polilinea chiusa

  AddItem(10,'0');
  AddItem(20,'0');
  AddItem(30,'0');
end;

constructor TDxfClass.Create;
begin
  FDxfFile:=TStringList.Create;
end;

destructor TDxfClass.Destroy;
begin
  FDxfFile.Free;
end;

procedure TDxfClass.EndOfFile;
begin
  AddItem(0,'EOF');
end;

procedure TDxfClass.EndPolyline;
begin
  AddItem(0,'SEQEND');
end;

procedure TDxfClass.EndSection;
begin
  AddItem(0,'ENDSEC');
end;

procedure TDxfClass.SaveToFile(const FileName: string);
begin
  FDxfFile.SaveToFile(FileName);
end;

end.
