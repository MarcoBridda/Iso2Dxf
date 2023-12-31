unit Iso2Dxf.Iso;
//Una unit per la parte Iso

interface

uses
  System.Classes;

type TIsoBlock = class
private
  FWords: TStringList;
  FComments: TStringList;
public
  constructor Create(aBlock: String);
  destructor Destroy; override;
end;

implementation

{ TIsoBlock }

constructor TIsoBlock.Create(aBlock: String);
begin
  FWords:=TStringList.Create;
  FComments:=TStringList.Create;
end;

destructor TIsoBlock.Destroy;
begin
  FComments.Free;
  FWords.Free;

  inherited;
end;

end.
