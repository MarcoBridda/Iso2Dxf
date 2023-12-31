unit Iso2Dxf.Iso;
//Una unit per la parte Iso

interface

uses
  System.Classes, System.SysUtils;

type TIsoBlock = class
private
  FBlock: String;
  FWords: TStringList;
  FComments: TStringList;

  //Utilità
  function ExtractComments(const aBlock: String): String;
  function Normalize(const aBlock: String): String;
  procedure ExtractWords(const aBlock: String);

  //Accesso alle proprietà
  procedure SetBlock(const Value: String);
public
  constructor Create(aBlock: String);
  destructor Destroy; override;

  property Block: String read FBlock write SetBlock;
end;

implementation

{ TIsoBlock }

constructor TIsoBlock.Create(aBlock: String);
begin
  FWords:=TStringList.Create;
  FComments:=TStringList.Create;

  Block:=aBlock;
end;

destructor TIsoBlock.Destroy;
begin
  FComments.Free;
  FWords.Free;

  inherited;
end;

function TIsoBlock.ExtractComments(const aBlock: String): String;
var
  PosB, PosE: Integer;
begin
  Result:=aBlock;
  FComments.Clear();

  PosB:=Result.IndexOf('(');

  while PosB>-1 do
  begin
    PosE:=Result.IndexOf(')',PosB+1);

    if PosE=-1 then
    begin
      FComments.Add(Result.Substring(PosB));
      Result:=Result.Remove(PosB);
    end
    else
    begin
      FComments.Add(Result.Substring(PosB,PosE-PosB+1));
      Result:=Result.Remove(PosB,PosE-PosB+1);
    end;

    PosB:=Result.IndexOf('(',PosE+1)
  end;
end;

procedure TIsoBlock.ExtractWords(const aBlock: String);
var
  Addr: Char;
  Text: String;
begin
  Text:=aBlock;

  for Addr:='A' to 'Z' do
    Text:=Text.Replace(Addr,#13#10+Addr+'=',[rfReplaceAll]);

  FWords.Text:=Text;

  //Eliminiamo la prima riga che è vuota dato che contiene solo <CR><LF>
  FWords.Delete(0)
end;

function TIsoBlock.Normalize(const aBlock: String): String;
begin
  Result:=aBlock.ToUpper().Replace(' ','',[rfReplaceAll]).Replace(
    #9,'',[rfReplaceAll]);
end;

procedure TIsoBlock.SetBlock(const Value: String);
var
  Text: String;
begin
  //Facciamo finta che il blocco iso sia sempre sintatticamente corretto
  Text:=ExtractComments(Value);
  Text:=Normalize(Text);
  ExtractWords(Text);

  FBlock := Value;
end;

end.
