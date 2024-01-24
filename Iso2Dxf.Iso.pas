unit Iso2Dxf.Iso;
//Una unit per la parte Iso

interface

uses
  System.Classes, System.SysUtils, Generics.Collections,
  Iso2Dxf.Utils;

type
  TIsoAddress = 'A'..'Z';
  TIsoWord = type String;
  TIsoWords = TList<TIsoWord>;

  TIsoWordHelper = record helper for TIsoWord
  private
    function GetAddress: Char;
    function GetValue: String;
    function GetFloatValue: Single;
    function GetIntValue: Integer;
  public
    //Per il momento le proprietà sono read-only
    property Address: Char read GetAddress;
    property Value: String read GetValue;

    //Tenta di convertire il valore da stringa a intero o float
    property IntValue: Integer read GetIntValue;
    property FloatValue: Single read GetFloatValue;
  end;

type
  TIsoBlock = class
  private
    FBlock: String;
    FWords: TIsoWords;
    FComments: TStringList;

    //Utilità
    function ExtractComments(const aBlock: String): String;
    function Normalize(const aBlock: String): String;
    procedure ExtractWords(const aBlock: String);

    //Accesso alle proprietà
    procedure SetBlock(const Value: String);
    function GetIsEmpty: Boolean;
  public
    constructor Create(aBlock: String = '');
    destructor Destroy; override;

    property Block: String read FBlock write SetBlock;
    property Comments: TStringList read FComments;
    property Words: TIsoWords read FWords;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

implementation

{ TIsoBlock }

constructor TIsoBlock.Create(aBlock: String);
begin
  FWords:=TIsoWords.Create;
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
  List: TArray<String>;
  Text, L: String;
begin
  Text:=aBlock;
  FWords.Clear;

  for Addr:='A' to 'Z' do
    Text:=Text.Replace(Addr,'@'+Addr,[rfReplaceAll]);

  List:=Text.Split(['@']);

  for L in List do
    FWords.Add(TIsoWord(L));

  FWords.Delete(0);
end;

function TIsoBlock.GetIsEmpty: Boolean;
begin
  Result:=Block.IsEmpty
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
  if not Value.IsEmpty then
  begin
    Text:=ExtractComments(Value);
    Text:=Normalize(Text);
    ExtractWords(Text);
  end
  else
  begin
    Words.Clear();
    Comments.Clear();
  end;

    FBlock:=Value
end;

{ TIsoWordHelper }

function TIsoWordHelper.GetAddress: Char;
begin
  Result:=String(Self)[1];
end;

function TIsoWordHelper.GetFloatValue: Single;
begin
  Result:=String(Self.Value).ToSingle();
end;

function TIsoWordHelper.GetIntValue: Integer;
begin
  Result:=String(Self.Value).ToInteger();
end;

function TIsoWordHelper.GetValue: String;
begin
  Result:=String(Self).Remove(0,1);
end;

end.
