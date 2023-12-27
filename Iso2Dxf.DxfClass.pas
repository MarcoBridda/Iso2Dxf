unit Iso2Dxf.DxfClass;
//Una classe per rappresentare un file dxf

interface

uses
  System.Classes, System.SysUtils;

type
  TDxfClass = class
  private
    FDxfFile: TstringList;

    procedure AddItem(const Code: Integer; const Value: String);
  public
    constructor Create;
    destructor Destroy;
  end;

implementation

{ TDxfClass }

procedure TDxfClass.AddItem(const Code: Integer; const Value: String);
begin
  FDxfFile.Add('  ' + Code.ToString);
  FDxfFile.Add(Value);
end;

constructor TDxfClass.Create;
begin
  FDxfFile:=TStringList.Create;
end;

destructor TDxfClass.Destroy;
begin
  FDxfFile.Free;
end;

end.
