unit Iso2Dxf.Iso;
//****************************************************************************
//Unit per la parte ISO
//
//****************************************************************************

interface

uses
  System.Classes, System.SysUtils,
  Generics.Collections,
  Iso2Dxf.Utils;

type
  //Eccezioni
  EIso2DxfIso = class(Exception);

  TIsoAddress = 'A'..'Z';
  TIsoWord = type String;
  TIsoWords = TList<TIsoWord>;

  TIsoWordHelper = record helper for TIsoWord
  private
    const
      CNC_AXES: TSysCharSet = ['A', 'B', 'C', 'U', 'V', 'W', 'X', 'Y', 'Z'];
      CNC_FUNCTIONS: TSysCharSet = ['F', 'G', 'M', 'N', 'S', 'T'];

    function GetAddress: Char;
    function GetValue: String;
    function GetFloatValue: Single;
    function GetIntValue: Integer;
    function GetStringValue: String;
  public
    //Un metodo che restituisce le impostazioni di stampa dei float
    function GetFloatSettings: TFloatSettings;

    //Per il momento le proprietà sono read-only
    property Address: Char read GetAddress;
    property Value: String read GetValue;

    //Tenta di convertire il valore da stringa a intero o float
    property IntValue: Integer read GetIntValue;
    property FloatValue: Single read GetFloatValue;
    //Conversione del valore in stringa usando le impostazioni di GetFloatSettings
    property StringValue: String read GetStringValue;
  end;

type
  TIsoBlock = class
  private
    FBlock: String;
    FWords: TIsoWords;
    FComments: TStringList;

    //Utilità
    function FindComment(const aBlock: String; out PosBegin, PosEnd: Integer;
      const StartIndex: Integer = 0): Boolean;
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

  //Enumeratore per la classe TIsoFile, in modo da poter scandire i blocchi
  //anche con un ciclo for-in
  TIsoFileEnumerator = class;

  TIsoFile = class
  private
    //Un file iso-cnc come lista generica di blocchi
    FBlocks: TList<TIsoBlock>;

    //Siccome gli elementi della lista sono oggetti, per pulirla non basta usare
    //il suo metodo Clear, ma bisogna anche distruggere manualmente tutti i suoi
    //elementi.
    procedure ClearBlocks;

    //Getter-Setter
    function GetBlock(Index: Integer): TIsoBlock;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    //Enumeratore
    function GetEnumerator: TIsoFileEnumerator;

    //Carica e salva dal disco
    procedure LoadFromFile(const aFileName: TFileName);
    procedure SaveToFile(const aFileName: TFileName);

    //Proprietà
    property Block[Index: Integer]: TIsoBlock read GetBlock;
    property Count: Integer read GetCount;
  end;

  //Implementazione effettiva dell'enumeratore per TIsoFile
  TIsoFileEnumerator = class
  private
    FContainer: TIsoFile;
    FIndex: Integer;
  public
    constructor Create(AContainer : TIsoFile);

    function GetCurrent: TIsoBlock;
    function MoveNext: Boolean;

    property Current: TIsoBlock read GetCurrent;
  end;

implementation

const
  //Messaggi di errore
  NON_COMPLIANT_COMMENT = 'Commento non conforme';

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

  PosB:=-1; PosE:=-1;

  while FindComment(Result, PosB, PosE) do
  begin
    FComments.Add(Result.Substring(PosB,PosE-PosB+1));
    Result:=Result.Remove(PosB,PosE-PosB+1);
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

  if not aBlock.IsEmpty then
  begin
    for Addr:='A' to 'Z' do
      Text:=Text.Replace(Addr,'@'+Addr,[rfReplaceAll]);

    List:=Text.Split(['@']);

    for L in List do
      FWords.Add(TIsoWord(L));

    FWords.Delete(0);
  end;
end;

function TIsoBlock.FindComment(const aBlock: String; out PosBegin,
  PosEnd: Integer; const StartIndex: Integer): Boolean;
var
  Count: Integer;
begin
  //Cerchiamo la prima parentesi aperta partendo da StartIndex
  PosBegin:=aBlock.IndexOf('(', StartIndex);
  //Il metodo ritorna vero se ha trovato un commento
  Result:=false;

  //Poi cerchiamo una parentesi chiusa tenendo conto degli annidamenti
  if PosBegin>-1 then
  begin
   PosEnd:=PosBegin+1;
   Count:=0;
   while (PosEnd<Length(aBlock)) and not Result do
   begin
     if aBlock[PosEnd] = '(' then
       Inc(Count);
     if aBlock[PosEnd] = ')' then
     begin
       Result:=Count=0;
       Dec(Count)
     end;
     Inc(PosEnd);
   end;

   //Se abbiamo trovato una parentesi aperta ma non quella chiusa corrispondente
   //segnala il problema
   if not Result and (PosEnd = Length(aBlock)) then
     raise EIso2DxfIso.Create(NON_COMPLIANT_COMMENT);
  end;
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

function TIsoWordHelper.GetFloatSettings: TFloatSettings;
begin
  Result:=TFloatSettings.Default
end;

function TIsoWordHelper.GetFloatValue: Single;
begin
  Result:=GetFloatSettings.StrToFloat(String(Self.Value))
end;

function TIsoWordHelper.GetIntValue: Integer;
begin
  Result:=Round(Self.FloatValue)
end;

function TIsoWordHelper.GetStringValue: String;
begin
  if CharInSet(Self.Address,CNC_AXES) then
    //Se l'indirizzo corrisponde al nome di un asse, formatta in float,
    Result:=GetFloatSettings.FloatToStr(Self.GetFloatValue)
  else if CharInSet(Self.Address,CNC_FUNCTIONS) then
    //Se sono funzioni macchina formatta come intero
    Result:=Self.GetIntValue.ToString
  else
    //Altrimenti restituisci così com'è
    Result:=Self.Value
end;

function TIsoWordHelper.GetValue: String;
begin
  Result:=String(Self).Remove(0,1);
end;

{ TIsoFile }

procedure TIsoFile.ClearBlocks;
begin
  while FBlocks.Count>0 do
    FBlocks.ExtractAt(FBlocks.Count-1).Free
end;

constructor TIsoFile.Create;
begin
  FBlocks:=TList<TIsoBlock>.Create;
end;

destructor TIsoFile.Destroy;
begin
  ClearBlocks;
  FBlocks.Free;
  inherited;
end;

function TIsoFile.GetBlock(Index: Integer): TIsoBlock;
begin
  Result:=FBlocks[Index]
end;

function TIsoFile.GetCount: Integer;
begin
  Result:=FBlocks.Count
end;

function TIsoFile.GetEnumerator: TIsoFileEnumerator;
begin
  Result:=TIsoFileEnumerator.Create(self)
end;

procedure TIsoFile.LoadFromFile(const aFileName: TFileName);
var
  Lines: TStringList;
  Line: String;
begin
  Lines:=TStringList.Create;

  try
    Lines.LoadFromFile(aFileName);

    ClearBlocks;
    for Line in Lines do
      FBlocks.Add(TIsoBlock.Create(Line))

  finally
    Lines.Free
  end;
end;

procedure TIsoFile.SaveToFile(const aFileName: TFileName);
var
  Lines: TStringList;
  Block: TIsoBlock;
begin
  Lines:=TStringList.Create;

  try
    for Block in FBlocks do
      Lines.Add(Block.Block);

    Lines.SaveToFile(aFileName);
  finally
    Lines.Free
  end;
end;

{ TIsoFileEnumerator }

constructor TIsoFileEnumerator.Create(AContainer: TIsoFile);
begin
  FContainer:=AContainer;
  FIndex:=-1
end;

function TIsoFileEnumerator.GetCurrent: TIsoBlock;
begin
  Result:=FContainer.Block[FIndex]
end;

function TIsoFileEnumerator.MoveNext: Boolean;
begin
  Result:=FIndex<FContainer.Count-1;

  if Result then
    Inc(FIndex)
end;

end.
