unit Execute.XML.Tree;

{
  XML Unit for Delphi Tokyo (c)2017 by Execute SARL
  http://www.execute.fr

  v1.0 - 2017-08-12
  v1.1 - 2017-08-13 error conditions
  v1.2 - 2017-08-14 include TextNodes (name.len = 0, name = '')
  v1.3 - 2017-08-18 added "xml" node, new XML Execptions and AnsiValue for non-UTF8 XML
  v1.4 - 2017-09-06 improved XPath support

  see Test() procedure below
}

interface

uses
  System.SysUtils;

type
  EXMLError = class(Exception)
    Source : PAnsiChar;
    Index  : Integer;
    constructor Create(const AMsg: string; ASource: PAnsiChar; AIndex: Integer);
  end;

  EXPathError = class(Exception)
    Path  : UTF8String;
    Index : Integer;
    constructor Create(const AMsg: string; APath: UTF8String; AIndex: Integer);
  end;

  // not an XML source
  EXMLNotXMLError = class(EXMLError);
  // expected string not found (or unexpected string found)
  EXMLExpectedStringNotFoundError = class(EXMLError)
    Expected: string;
    constructor Create(const AExpected: string; ASource: PAnsiChar; AIndex: Integer);
  end;
  // unexpected End of XML
  EXMLEndOfXMLError = class(EXMLError);
  // unexpected /tag
  EXMLUnexpectedClosingTagError = class(EXMLError);
  // expected /tag not found
  EXMLExpectedClosingTagNotFoundError = class(EXMLError);

  // reference to a String in the XML source
  TXMLString = record
    // pointer to the first character
    Start: PAnsiChar;
    // length of the string
    Len  : Integer; // v1.3 byte => integer
    // search for a char in the string
    function Pos(Ch: AnsiChar): Integer;
    // compare the string with a given PAnsiChar
    function Compare(const AStr: PAnsiChar; ALen: Integer): Boolean;
    // compare the string with an other TXMLString
    function Equals(const AName: TXMLString): Boolean; inline;
    // compare the string with an UTF8String
    function Match(const AStr: UTF8String): Boolean; inline;
    // compare the string with an UTF8String - case insensitive
    function iMatch(const AStr: UTF8String): Boolean; inline;
    // return the value of the TXMLString, safe to call on a "nil" PXMLString
    function Value: UTF8String;
    // return the value from a ISO XML file that is NOT UTF8 encoded !
    function AnsiValue: AnsiString;
  end;
  PXMLString = ^TXMLString;

  // references to the name and the value of an attribute in the XML source
  TXMLAttribute = record
    Name : TXMLString;
    Value: TXMLString;
  end;

  // an XML Node is defined by a pair of tag : <tag>..</tag> - or a single tag <tag/>
  PXMLNode = ^TXMLNode;
  TXMLNode = record
  private
    type
      TSelector = record
        Path : UTF8String;
        Index: Integer;
        Name : UTF8String;
        Attr : UTF8String;
        Value: UTF8String;
        Nth  : Integer;
        Any  : Boolean;
        function Skip(c: AnsiChar): Boolean;
        procedure Drop(c: AnsiChar);
        procedure NextChar;
        procedure Next(const Source: TSelector);
        function Match(var Node: PXMLNode): Boolean;
        function EndOfPath: Boolean;
      end;
    function Walk(var ASelector: TSelector; var ANode: PXMLNode): Boolean;
    function Select(var ASelector: TSelector; var ANode: PXMLNode): Boolean;
  public
  // name of the tag
    Name    : TXMLString;
  // attributs of the tag <tag name="value"...>
    Attrs   : TArray<TXMLAttribute>;
  // text inside the tag <tag>Text</tag>
    Text    : TXMLString;
  // children XML Node of this one (including Text node)
    Children: TArray<TXMLNode>;
  // number of children
    function Length: Integer; inline;
  // retrieve nth child of a given name (direct child only)
    function GetNthChild(const AName: UTF8String; ANth: Integer): PXMLNode;
  // retreive a child by it's name (direct child only)
    function GetChild(const AName: UTF8String): PXMLNode;
    function HasChild(const AName: UTF8String; var AChild: PXMLNode): Boolean;
  // retreive a child by it's path (allows sub child)
    function GetChildAt(const APath: array of UTF8String): PXMLNode;
    function HasChildAt(const APath: array of UTF8String; var AChild: PXMLNode): Boolean;
  // retreive a child at any depth
    function Search(const AName: UTF8String; Nth: Integer = 1): PXMLNode;
  // retreive an attribute by it's name
    function GetAttribute(const AName: UTF8String): PXMLString;
    function HasAttribute(const AName: UTF8String; var AAttr: PXMLString): Boolean;
  // XPath syntax
  // for XPathAttr use XPathNode('...').Attribute['name']
    function XPathNode(const APath: UTF8String): PXMLNode;
  // return the value of Text, safe to call on a "nil" PXMLNode
    function Value: UTF8String; inline;
    // return the value from a ISO XML file that is NOT UTF8 encoded !
    function AnsiValue: AnsiString; inline;
  // properties
    property Child[const AName: UTF8String]: PXMLNode read GetChild; default;
    property Attribute[const AName: UTF8String]: PXMLString read GetAttribute;
  end;

  // collection of TXMLNode
  TXMLTree = record
  private
  // the XML source
    Text       : UTF8String; // keep the RefCount of the text > 0
    PText      : PAnsiChar;
    Size       : Integer;
  // position inside Text
    Position   : Integer;
  // readed TagName
    TagName    : TXMLString;
  // is TagName prefixed with / ?
    EndTag     : Boolean;
  // data between the opening tag and the closing one
    DataLen    : Integer;
  // make sure a node is empty
    procedure ClearNode(var Node: TXMLNode);
  // init things
    procedure Init(AText: PAnsiChar; ALen: Integer);
  // skip spaces
    procedure Spaces;
  // read a TagName
    function GetTag(var Parent: TXMLNode): Boolean;
  // add a TextNode
    procedure TextNode(var Parent: TXMLNode; Len: Integer);
  // read node Attributes
    procedure GetAttrs(var Node: TXMLNode);
  // read a TXMLNode
    procedure GetNode(var Node: TXMLNode);
  // read a child node
    function ChildTag(var Node: TXMLNode): Boolean;
  // read a TXMLAttribute
    procedure GetAttr(var Attr: TXMLAttribute);
  // read until Seq and return the number of characters skiped
    function ReadUntil(const Seq: UTF8String; var Skipped: Integer): Boolean;
  // skip a char and raise an Exception at the end of XML
    procedure DropChar;
  // skip a sequence of chars if possible
    function Skip(const Str: UTF8String): Boolean;
  // drop a sequence of chars or raise an Exception
    procedure Drop(const Str: UTF8String);
  public
  // "<?xml version="1.0" encoding="..."?>
    xml : TXMLNode; // to detect encoding
  // the Root node
    Root: TXMLNode;
  // parse AText and build the node Tree
    procedure Build(const AText: UTF8String);
  // build from a PAnsiChar and a char count
    procedure BuildFrom(const AText: PAnsiChar; ALen: Integer);
  // parse a multi root structure (special need)
    procedure BuildMultiRoot(const AText: PAnsiChar; ALen: Integer);
  // retreive a Node at the given path
    function GetNodeAt(const APath: array of string): PXMLNode;
    function HasNodeAt(const APath: array of string; var ANode: PXMLNode): Boolean;
  // limited XPath syntax
  //   //           : any node
  //   parent/child : child node
  //   child[x]     : x-nth "child" node
  //   [x]          : x-nth children
  // added in version 1.4
  //   child[@name]       : "child" with a "name" attribute
  //   child[@name=value] : "child" with a "name" attribute and a value "value"
  //   child[x@name=value]: nth "child" with a "name" attribute and a value "value"
  // todo (or not todo) :
  //   child[@*]          : "child" with at least one attribute
  //   child[last()]      : last "child"
  //   child[last()-1]    : last but one "child"
  //   ...
    function XPathNode(const APath: UTF8String): PXMLNode;
  //   <path>/@name : "name" attribute
    function XPathAttr(const APath: UTF8String): PXMLString;
  end;

  // Search inside a TXMLTree
  TXMLSearch = record
  private type
  // search state
    TSearchNode = record
      Node : PXMLNode;
      Child: Integer;
    end;
  private
  // searched name
    Name : UTF8String;
  // search stack
    Stack: TArray<TSearchNode>;
  // actual depth of the Stack
    Depth: Integer;
  public
    Node: PXMLNode;
    function FindFirst(const ATree: TXMLTree; const AName: UTF8String): Boolean; inline;
    function FindNode(AStart: PXMLNode; const AName: UTF8String): Boolean;
    function FindNext: Boolean;
    procedure FindClose;
  end;

function StrToIntDef(const Str: RawByteString; Default: Integer): Integer;

implementation

resourcestring
  sUnexpectedEndOfXML   = 'Unexpected end of XML';
  sUnexpectedClosingTag = 'Unexpected closing tag "/%s"';
  sExpectedClosingTag   = 'Expected tag "/%s" not found';
  sExpectedString       = 'Expected string not found';

function StrToIntDef(const Str: RawByteString; Default: Integer): Integer;
var
  Index: Integer;
  Value: Integer;
begin
  if Str = '' then
    Exit(Default);
  Result := 0;
  for Index := 1 to Length(Str) do
  begin
    Value := Ord(Str[Index]) - Ord('0');
    if Value in [0..9] then
      Result := 10 * Result + Value
    else
      Exit(Default);
  end;
end;

{ TXMLNode.TSelector }

function TXMLNode.TSelector.Skip(c: AnsiChar): Boolean;
begin
  Result := Path[Index] = c;
  if Result then
    Inc(Index);
end;

procedure TXMLNode.TSelector.Drop(c: AnsiChar);
begin
  if not Skip(c) then
    raise EXPathError.Create('Unpexected character', Path, Index);
end;

procedure TXMLNode.TSelector.NextChar;
begin
  if Index >= Length(Path) then
    raise EXPathError.Create('Unexpected end of path', Path, Index);
  Inc(Index);
end;

function TXMLNode.TSelector.EndOfPath: Boolean;
begin
  Result := Index > Length(Path);
end;

function TXMLNode.TSelector.Match(var Node: PXMLNode): Boolean;
var
  Str: PXMLString;
begin
  if Any then
    Result := Node.Walk(Self, Node)
  else begin
    Result := False;
    if (Name <> '') and not Node.Name.Match(Name) then
      Exit;
    if (Attr <> '') then
    begin
      Str := Node.Attribute[Attr];
      if Str = nil then
        Exit;
      if (Value <> '') and not Str.Match(Value) then
        Exit;
    end;
    if (Nth = -1) or (Nth = 1) then
    begin
      Result := Node.Walk(Self, Node);
      Exit;
    end;
    if Nth > 0 then
    begin
      Dec(Nth);
    end;
  end;
end;

procedure TXMLNode.TSelector.Next(const Source: TSelector);
var
  Start: Integer;
begin
  Path := Source.PAth;
  Index := Source.Index;
  Start := Index;
  Nth := -1;
  Attr := '';
  Value := '';
  Any := False; // allows Next(Self)
  if Skip('/') then
  begin
    if Source.Any then
      raise EXPathError.Create('Invalid Path', Path, Index);
    Any := True;
    Exit;
  end;
  while (Index <= Length(Path)) and not (Path[Index] in ['[', '/']) do
  begin
    Inc(Index);
  end;
  Name := Copy(Path, Start, Index - Start);
  if Path[Index] = '[' then
  begin
    Inc(Index);
    if Path[Index] <> '@' then
    begin
      Nth := 0;
      while Path[Index] in ['0'..'9'] do
      begin
        Nth := 10 * Nth + Ord(Path[Index]) - Ord('0');
        Inc(Index);
      end;
    end;
    if Path[Index] = '@' then
    begin
      Inc(Index);
      Start := Index;
      while not (Path[Index] in ['=', ']']) do
      begin
        NextChar;
      end;
      Attr := Copy(Path, Start, Index - Start);
      if Path[Index] = '=' then
      begin
        Inc(Index);
        Start := Index;
        while Path[Index] <> ']' do
        begin
          NextChar;
        end;
        Value := Copy(Path, Start, Index - Start);
      end;
    end;
    Drop(']');
  end;
  if Index < Length(Path) then
  begin
    Drop('/');
  end;
end;

procedure GetName(const APath: UTF8String; var Index: Integer; var Name, Attr, Value: UTF8STring; var Nth: Integer);
var
  Start: Integer;

  procedure Drop(c: AnsiChar);
  begin
    if APath[Index] <> c then
      raise EXPathError.Create('Unpexected character', APath, Index);
    Inc(Index);
  end;

  procedure NextChar;
  begin
    if Index = Length(APath) then
      raise EXPathError.Create('Unexpected end of path', APath, Index);
    Inc(Index);
  end;

begin
  Start := Index;
  Nth := -1;
  Attr := '';
  Value := '';
  while (Index <= Length(APath)) and not (APath[Index] in ['[', '/']) do
  begin
    Inc(Index);
  end;
  Name := Copy(APath, Start, Index - Start);
  if APath[Index] = '[' then
  begin
    Inc(Index);
    if APath[Index] <> '@' then
    begin
      Nth := 0;
      while APath[Index] in ['0'..'9'] do
      begin
        Nth := 10 * Nth + Ord(APath[Index]) - Ord('0');
        Inc(Index);
      end;
    end;
    if APath[Index] = '@' then
    begin
      Inc(Index);
      Start := Index;
      while not (APath[Index] in ['=', ']']) do
      begin
        NextChar;
      end;
      Attr := Copy(APath, Start, Index - Start);
      if APath[Index] = '=' then
      begin
        Inc(Index);
        Start := Index;
        while APath[Index] <> ']' do
        begin
          NextChar;
        end;
        Value := Copy(APath, Start, Index - Start);
      end;
    end;
    Drop(']');
  end;
  if Index < Length(APath) then
  begin
    Drop('/');
  end;
end;

{ EXMLError }

constructor EXMLError.Create(const AMsg: string; ASource: PAnsiChar; AIndex: Integer);
begin
  inherited Create(AMsg);
  Source := ASource;
  Index := AIndex;
end;

{ EXPathError }

constructor EXPathError.Create(const AMsg: string; APath: UTF8String; AIndex: Integer);
begin
  inherited Create(AMsg);
  Path := APath;
  Index := AIndex;
end;

{ EXMLExpectedStringNotFoundError }

constructor EXMLExpectedStringNotFoundError.Create(const AExpected: string;
  ASource: PAnsiChar; AIndex: Integer);
begin
  inherited Create(sExpectedString, ASource, AIndex);
  Expected := AExpected;
end;

{ TXMLString }

function TXMLString.Pos(Ch: AnsiChar): Integer;
var
  Index: Integer;
begin
  for Index := 0 to Len - 1 do
    if Start[Index] = Ch then
      Exit(Index);
  Result := -1;
end;

function TXMLString.Compare(const AStr: PAnsiChar; ALen: Integer): Boolean;
var
  Index: Integer;
begin
  Result := False;
  if ALen <> Len then
    Exit;
  for Index := 0 to Len - 1 do
  begin
   if AStr[Index] <> Start[Index] then
     Exit;
  end;
  Result := True;
end;

function TXMLString.Equals(const AName: TXMLString): Boolean;
begin
  Result := Compare(AName.Start, AName.Len);
end;

function TXMLString.Match(const AStr: UTF8String): Boolean;
begin
  Result := Compare(Pointer(AStr), Length(AStr));
end;

function TXMLString.iMatch(const AStr: UTF8String): Boolean;
var
  Index: Integer;
  Src  : PAnsiChar;
begin
  Result := False;
  if Length(AStr) <> Len then
    Exit;
  Src := PAnsiChar(AStr);
  for Index := 0 to Len - 1 do
  begin
   if UpCase(Src[Index]) <> Start[Index] then
     Exit;
  end;
  Result := True;
end;

function TXMLString.Value: UTF8String;
begin
  if (@Self = nil) or (Len = 0) then
    Result := ''
  else begin
    SetLength(Result, Len);
    Move(Start^, Result[1], Len);
  end;
end;

function TXMLString.AnsiValue: AnsiString;
begin
  if (@Self = nil) or (Len = 0) then
    Result := ''
  else begin
    SetLength(Result, Len);
    Move(Start^, Result[1], Len);
  end;
end;

{ TXMLNode }

function TXMLNode.Length: Integer;
begin
  if @Self = nil then
    Result := 0
  else
    Result := System.Length(Children);
end;

function TXMLNode.GetNthChild(const AName: UTF8String; ANth: Integer): PXMLNode;
var
  Index: Integer;
begin
  if @Self <> nil then
  begin
    for Index := 0 to Length - 1 do
    begin
      Result := @Children[Index];
      if Result.Name.Match(AName) then
      begin
        Dec(ANth);
        if ANth <= 0 then
          Exit;
      end;
    end;
  end;
  Result := nil;
end;

function TXMLNode.GetChild(const AName: UTF8String): PXMLNode;
begin
  Result := GetNthChild(AName, 1);
end;

function TXMLNode.HasChild(const AName: UTF8String; var AChild: PXMLNode): Boolean;
begin
  AChild := GetChild(AName);
  Result := AChild <> nil;
end;

function TXMLNode.GetChildAt(const APath: array of UTF8String): PXMLNode;
var
  Index: Integer;
begin
  Result := @Self;
  if Result = nil then
    Exit;
  for Index := 0 to System.Length(APath) - 1 do
  begin
    Result := Result.GetChild(APath[Index]);
    if Result = nil then
      Exit;
  end;
end;

function TXMLNode.HasChildAt(const APath: array of UTF8String; var AChild: PXMLNode): Boolean;
begin
  AChild := GetChildAt(APath);
  Result := AChild <> nil;
end;

function TXMLNode.Search(const AName: UTF8String; Nth: Integer = 1): PXMLNode;
var
  SR: TXMLSearch;
begin
  if SR.FindNode(@Self, AName) then
  begin
    while (Nth > 1) and SR.FindNext do
      Dec(Nth);
    Result := SR.Node
  end else begin
    Result := nil;
  end;
end;

function TXMLNode.GetAttribute(const AName: UTF8String): PXMLString;
var
  Index: Integer;
begin
  if @Self <> nil then
  begin
    for Index := 0 to System.Length(Attrs) - 1 do
    begin
      if Attrs[Index].Name.Match(AName) then
        Exit(@Attrs[Index].Value);
    end;
  end;
  Result := nil;
end;

function TXMLNode.HasAttribute(const AName: UTF8String; var AAttr: PXMLString): Boolean;
begin
  AAttr := GetAttribute(AName);
  Result := AAttr <> nil;
end;

function TXMLNode.Value: UTF8String;
begin
  if @Self = nil then
    Result := ''
  else
    Result := Text.Value;
end;

function TXMLNode.Walk(var ASelector: TSelector; var ANode: PXMLNode): Boolean;
var
  Index   : Integer;
  Selector: TSelector;
begin
  if ASelector.EndOfPath then
    Exit(ANode <> nil);
  Selector.Next(ASelector);
  if ASelector.Any then
  begin
    Result := ANode.Select(Selector, ANode);
  end else begin
    for Index := 0 to Length - 1 do
    begin
      ANode := @Children[Index];
      if Selector.Match(ANode) then
        Exit(True);
    end;
    Result := False;
  end;
end;

function TXMLNode.Select(var ASelector: TSelector; var ANode: PXMLNode): Boolean;
var
  Index: Integer;
begin
  if ASelector.Match(ANode) then
    Exit(True);
  for Index := 0 to Length - 1 do
  begin
    ANode := @Children[Index];
    if ANode.Select(ASelector, ANode) then
      Exit(True);
  end;
  Result := False;
end;

function TXMLNode.XPathNode(const APath: UTF8String): PXMLNode;
var
  Selector: TSelector;
begin
  if @Self = nil then
    Exit(nil);

  if APath = '' then
    Exit(@Self);

  Selector.Path := APath;
  Selector.Index := 1;
  Selector.Skip('/');
  Result := @Self;
  if not Selector.Match(Result) then
//  if Walk(Selector, Result) = False then
    Result := nil;
end;

function TXMLNode.AnsiValue: AnsiString;
begin
  if @Self = nil then
    Result := ''
  else
    Result := Text.AnsiValue;
end;

{ TXMLTree }

procedure TXMLTree.Spaces;
begin
  while (PText[Position] in [#0, #9, #10, #13, ' ']) do
    DropChar;
end;

function TXMLTree.GetTag(var Parent: TXMLNode): Boolean;
var
  start: Integer;
  len  : Integer;
  dots : Integer;
begin
  TagName.Len := 0;
// initial start of the tag
  start := Position;
  repeat
  // reset DataLen if we have skipped comments etc...
    DataLen := Position - Start;
  // count chars until first "<"
    Result := ReadUntil('<', len);
    if Len > 0 then
      TextNode(Parent, len);
    Inc(DataLen, len);
    if Result = False then
      Exit;
  // i<?...?> tags
    if Skip('?') then
    begin
      if Skip('xml ') then // <?xml version="1.0" encoding="UTF-8"?>
      begin
       // specify TagName
        TagName.Start := @PText[Position - 5];
        TagName.Len := 4;
       // set Name and read Attributes
        GetAttrs(xml);
       // reset TagName for the repeat loop
        TagName.Len := 0;
      end;
      // read until end of tag
      while not Skip('?>') do
        DropChar;
    end else
  // ignore comments like <!-- comment -->
    if Skip('!--') then
    begin
      while not Skip('-->') do
        Inc(Position);
    end else begin
    // skip "/" in </tag>
      EndTag := PText[Position] = '/';
      if EndTag then
        Inc(Position);
    // read name
      TagName.Start := @PText[Position];
      TagName.Len := Position;
      while (not (PText[Position] in [#9, #10, #13, ' ', '/', '>'])) do
      begin
        DropChar;
      end;
    // compute TagName len
      TagName.Len := Position - TagName.Len;
    // drop Namespace <soap:Envelope> => "Envelope"
      dots := TagName.Pos(':') + 1;
      if dots > 0 then
      begin
        Inc(TagName.Start, Dots);
        Dec(TagName.Len, Dots);
      end;
    end;
  until TagName.Len > 0;
end;

procedure TXMLTree.TextNode(var Parent: TXMLNode; Len: Integer);
var
  Start: Integer;
  Index: Integer;
  Node : Integer;
begin
  Start := Position - len - 1; // Position is after '<'
  for Index := Start to Start + Len - 1 do
  begin
    if not (PText[Index] in [#9, #10, #13, ' ']) then
    begin
      Node := Length(Parent.Children);
      SetLength(Parent.Children, Node + 1);
      Parent.Children[Node].Name.Start := nil;
      Parent.Children[Node].Name.Len := 0;
      Parent.Children[Node].Text.Start := @PText[Start];
      Parent.Children[Node].Text.Len := Len;
      Break;
    end;
  end;
  // ignore empty nodes
end;

procedure TXMLTree.GetAttrs(var Node: TXMLNode);
var
  Count: Integer;
begin
// set Name
  Node.Name := TagName;
// Attributes
  Spaces;
  Count := 0;
  while (Position < Size) and (not (PText[Position] in ['?', '/', '>'])) do
  begin
    SetLength(Node.Attrs, Count + 1);
    GetAttr(Node.Attrs[Count]);
    Inc(Count);
  end;
  Node.Text.Len := 0;
end;

procedure TXMLTree.GetNode(var Node: TXMLNode);
var
  Start: Integer;
  Count: Integer;
begin
  if EndTag then
    raise EXMLUnexpectedClosingTagError.Create(Format(sUnexpectedClosingTag, [string(TagName.Value)]), PText, Position - TagName.Len - 1);
// set Name and read Attributes
  GetAttrs(Node);
// Empty node
  if not Skip('/>') then
  begin
    Drop('>');
    Node.Text.Start := @PText[Position];
    // to compute Node.Text.Len
    Start := Position;
    while ChildTag(Node) do
    begin
      Count := Length(Node.Children);
      SetLength(Node.Children, Count + 1);
      GetNode(Node.Children[Count]);
      // adjust Text length
      Node.Text.Len := Position - Start;
    end;
    // DataLen is between the last child and "</tag>"
    Inc(Node.Text.Len, DataLen);
    Drop('>');
  end;
end;

function TXMLTree.ChildTag(var Node: TXMLNode): Boolean;
begin
  if GetTag(Node) = False then
    raise EXMLExpectedClosingTagNotFoundError.Create(Format(sExpectedClosingTag, [string(Node.Name.value)]), PText, (Node.Name.Start - PText));
  Result := (EndTag = False) or (TagName.Equals(Node.Name) = False);
end;

procedure TXMLTree.GetAttr(var Attr: TXMLAttribute);
var
  Quote: AnsiChar;
begin
// Attribute name
  Attr.Name.Start := @PText[Position];
  Attr.Name.Len := Position;
  while not(PText[Position] in [#9, #10, #13, ' ', '=']) do
  begin
    DropChar;
  end;
  Attr.Name.Len := Position - Attr.Name.Len;
// name =
  Spaces;
  Drop('=');
  Spaces;
// "value" or 'value'
  Quote := PText[Position];
  if Quote in ['"', ''''] then
    DropChar
  else
    Drop('"');
  Attr.Value.Start := @PText[Position];
  if ReadUntil(Quote, Attr.Value.Len) = False then
    raise EXMLEndofXMLError.Create(sUnexpectedEndOfXML, PText, Position);
// prepare for next attribute
  Spaces;
end;

function TXMLTree.ReadUntil(const Seq: UTF8String; var Skipped: Integer): Boolean;
begin
  Skipped := Position;
  Result := True;
  while not Skip(Seq) do
  begin
    if Position = Size then
    begin
      Result := False;
      Break;
    end;
    Inc(Position);
  end;
  Skipped := Position - Skipped - Length(Seq);
end;

procedure TXMLTree.DropChar;
begin
  if Position = Size then
    raise EXMLEndOfXMLError.Create(sUnexpectedEndOfXML, PText, Position);
  Inc(Position);
end;

function TXMLTree.Skip(const Str: UTF8String): Boolean;
var
  Len  : Integer;
  Index: Integer;
begin
  Len := Length(Str);
  if Position + Len > Size then
    Exit(False);
  for Index := 0 to Len - 1 do
  begin
    if PText[Position + Index] <> Str[Index + 1] then
      Exit(False);
  end;
  Inc(Position, Len);
  Result := True;
end;

procedure TXMLTree.Drop(const Str: UTF8String);
begin
  if not Skip(Str) then
    raise EXMLExpectedStringNotFoundError.Create(Str, PText, Position);
end;

procedure TXMLTree.ClearNode(var Node: TXMLNode);
begin
  Node.Attrs := nil;
  Node.Children := nil;
  FillChar(Node, SizeOf(TXMLNode), 0);
end;

procedure TXMLTree.Init(AText: PAnsiChar; ALen: Integer);
begin
  Text := ''; // release previous source
  ClearNode(xml);
  ClearNode(Root);
  PText := AText;
  Size := ALen;
  Position := 0;
end;

procedure TXMLTree.Build(const AText: UTF8String);
begin
  BuildFrom(Pointer(AText), Length(AText));
  Text := AText; // RefCount
end;

procedure TXMLTree.BuildFrom(const AText: PAnsiChar; ALen: Integer);
begin
  Init(AText, ALen);
  if GetTag(Root) then
    GetNode(Root)
  else
    raise EXMLNotXMLError.Create('Not an XML string', PText, Position);
end;

procedure TXMLTree.BuildMultiRoot(const AText: PAnsiChar; ALen: Integer);
var
  Count: Integer;
begin
  Init(AText, ALen);
  Root.Text.Start := PText;
  Root.Text.Len := Size;
  while GetTag(Root) do
  begin
    Count := Length(Root.Children);
    SetLength(Root.Children, Count + 1);
    GetNode(Root.Children[Count]);
  end;
end;

function TXMLTree.GetNodeAt(const APath: array of string): PXMLNode;
var
  Index: Integer;
begin
  if (Length(APath) = 0) or (Root.Name.Match(UTF8String(APath[0])) = False) then
    Exit(nil);
  Result := @Root;
  for Index := 1 to Length(APath) - 1 do
  begin
    if Result.HasChild(UTF8String(APath[Index]), Result) = False then
      Exit;
  end;
end;

function TXMLTree.HasNodeAt(const APath: array of string; var ANode: PXMLNode): Boolean;
begin
  ANode := GetNodeAt(APath);
  Result := ANode <> nil;
end;

function TXMLTree.XPathNode(const APath: UTF8String): PXMLNode;
var
  Selector: TXMLNode.TSelector;
begin
  Result := nil;

  if APath = '' then
    Exit;

  Selector.Path := APath;
  Selector.Index := 1;
  Selector.Skip('/');
  Selector.Next(Selector);

  Result := @Root;
  if not Selector.Match(Result) then
    Result := nil;
end;

function TXMLTree.XPathAttr(const APath: UTF8String): PXMLString;
var
  Index: Integer;
  Node : PXMLNode;
begin
  if APath = '' then
    Exit(nil);
  Index := Pos(UTF8String('@'), APath);
  if Index = 0 then
    Exit(nil);
  if Index = 1 then
    Node := @Root
  else begin
    if APath[Index - 1] <> '/' then
      Exit(nil);
    Node := XPathNode(Copy(APath, 1, Index - 2));
  end;
  Result := Node.GetAttribute(Copy(APath, Index + 1, MaxInt));
end;

{ TXMLSearch }

function TXMLSearch.FindFirst(const ATree: TXMLTree; const AName: UTF8String): Boolean;
begin
  Result := FindNode(@ATree.Root, AName);
end;

function TXMLSearch.FindNode(AStart: PXMLNode; const AName: UTF8String): Boolean;
begin
  Name := AName;
  SetLength(Stack, 1);
  Depth := 0;
  Node := AStart;
  Stack[0].Node := Node;
  Stack[0].Child := 0;
  Result := Node.Name.Match(Name);
  if Result = False then
    Result := FindNext;
end;

function TXMLSearch.FindNext: Boolean;
var
  SNode: TSearchNode;
begin
  Node := nil;
  if Stack <> nil then
  begin
    while Depth >= 0 do
    begin
      SNode := Stack[Depth];
      while SNode.Child < Length(SNode.Node.Children) do
      begin
        if Depth + 1 = Length(Stack) then
        begin
          SetLength(Stack, 2 * (Depth + 1));
        end;
        Inc(Depth);
        SNode.Node := @SNode.Node.Children[SNode.Child];
        SNode.Child := 0;
        Stack[Depth] := SNode;
        if SNode.Node.Name.Match(name) then
        begin
          Node := SNode.Node;
          Exit(True);
        end;
      end;
      Dec(Depth);
      Inc(Stack[Depth].Child);
    end;
    FindClose;
  end;
  Result := False;
end;

procedure TXMLSearch.FindClose;
begin
  Stack := nil;
  Depth := -1;
  Node := nil;
end;

procedure test;
type
  Ansi1252 = type AnsiString(1252);
var
  s: UTF8String;
  t: TXMLTree;
  n: PXMLNode;
  r: TXMLSearch;
  a: Ansi1252;
begin
  s := '<?xml version="1.0" encoding="UTF-8"?>'
     + '<root id="test">'
     + ' <xx:child name="first child">'
     + '  <xx:code name="code of first child">value for code of first child</xx:code>'
     + ' </xx:child>'
     + ' <child name="empty child"/>'
     + ' <child>plain text</child>'
     + ' <code>root code</code>'
     + ' <child code="attribute">'
     + '  <dummy/>'
     + '  <dummy>'
     + '   <code name="dummy1"/>'
     + '  </dummy>'
     + '  <code name="dummy2"/></child>'
     + '</root>';

  t.Build(s);

  assert(t.root.Length = 5);

  assert(t.root['child'].attribute['name'].value = 'first child');

  assert(t.root['nowhere']^['nowhere'].attribute['nowhere'].value = '');

  n := t.GetNodeAt(['root', 'child', 'code']);
  assert(n <> nil);
  assert(n.GetAttribute('name').value = 'code of first child');

  assert(r.FindFirst(t, 'code'));
  assert(r.Node <> nil);
  assert(r.Node.GetAttribute('name').value = 'code of first child');

  assert(r.FindNext);
  assert(r.Node <> nil);
  assert(r.Node.value = 'root code');

  assert(r.FindNext);
  assert(r.Node <> nil);
  assert(r.Node.GetAttribute('name').value = 'dummy1');

  assert(r.FindNext);
  assert(r.Node <> nil);
  assert(r.Node.GetAttribute('name').value = 'dummy2');

  assert(r.FindNext = False);
  assert(r.Node = nil);
  assert(r.Node.value = '');

  n := t.XPathNode('/root/child[4]/code');
  // return the child "code" node of the 4th "child" node of root
  assert(n <> nil);
  assert(n.attribute['name'].value = 'dummy2');

  n := t.XPathNode('/root/[5]//code');
  // return any "code" node from the 5th node of root
  assert(n <> nil);
  assert(n.attribute['name'].value = 'dummy1');

  // same as above, return the same node
  assert(n = t.XPathNode('root/child[4]/dummy[2]/code'));

  // return attribute
  assert(t.XPathAttr('root/child[4]/dummy[2]/code/@name').value = 'dummy1');

  // "name" attribute of the first "code" node
  assert(t.XPathAttr('//code/@name').value = 'code of first child');

  // "name" attribute of the 4th "code" node
  assert(t.XPathAttr('//code[4]/@name').value = 'dummy2');

  // 2017-08-13, error conditions

  s := 'not an valid XML string <root><dummy>wrong order</root></dummy>';
  try
    t.Build(s);
  except
    on e: EXMLUnexpectedClosingTagError do
    begin
      assert(e.Message = 'Unexpected closing tag "/root"');
      assert(Copy(string(e.source), e.Index + 1, 5) = '/root');
    end;
  end;

  s := 'not a valid XML string <root>open node...';
  try
    t.Build(s);
  except
    on e: EXMLExpectedClosingTagNotFoundError do
      assert(e.Message = 'Expected tag "/root" not found');
  end;

  s := 'not an XML string at all !';
  try
    t.Build(s);
  except
    on e: EXMLNotXMLError do
      assert(e.Message = 'Not an XML string');
  end;

  // 2017-08-18, text nodes

  s := '<root>text node and <child>'#13'<empty/>'#9' '#10'</child> end text</root>';
  t.Build(s);
  assert(t.root.Length = 3);
  // children[0] is a Text node
  assert(t.root.children[0].name.value = '');
  assert(t.root.children[0].text.value = 'text node and ');
 // children[1] has only 1 child, no text node
  assert(t.root.children[1].name.value = 'child');
  assert(t.root.children[1].Length = 1);
  assert(t.root.children[1].text.value = #13'<empty/>'#9' '#10);
 // last node is a Text node
  assert(t.root.children[2].name.value = '');
  assert(t.root.children[2].text.value = ' end text');

  // comments do split text nodes
  s := '<root> begin <!-- ignored --> end </root>';
  t.Build(s);
  assert(t.root.Length = 2);
  assert(t.root.children[0].name.len = 0);
  assert(t.root.children[0].text.value = ' begin ');
  assert(t.root.children[1].name.len = 0);
  assert(t.root.children[1].text.value = ' end ');

  // 2017-08-18 - new exception classes, xml node, .AnsiValue for iso encoded XML source

  // NB: "a" is an AnsiString(1252) not an UTF8STring !
  a := '<?xml version="1.0" encoding="ISO-8859-1"?><root><summer>été</summer></root>';
  t.BuildFrom(Pointer(a), Length(a));
  assert(t.xml.Attribute['encoding'].Value = 'ISO-8859-1');
  assert(t.Root['summer'].AnsiValue = 'été');

  // AnsiValue on UTF8
  s := '<?xml version="1.0" encoding="utf-8"?><root><summer>été</summer></root>';
  t.BuildFrom(Pointer(s), Length(s));
  assert(t.xml.Attribute['encoding'].Value = 'utf-8');
  assert(t.Root['summer'].AnsiValue = 'Ã©tÃ©');

  s := '<root attr=''wrong">';
  try
    t.Build(s);
  except
    on e: EXMLEndOfXMLError do
    begin
      assert(e.message = 'Unexpected end of XML');
    end;
  end;

  s := '<root attr=wrong>';
  try
    t.Build(s);
  except
    on e: EXMLExpectedStringNotFoundError do
    begin
      assert(e.Message = 'Expected string not found');
      assert(e.Expected = '"');
      assert(e.source[e.index] = 'w');
    end;
  end;

  // 2017-09-05: Node.XPathNode
  s := '<root><child><test>ici</test></child><child><test flag="1">there</test><test flag="2">2nd</test></child><child><subchild value="yes">found</subchild><test flag="value">three</test></child></root>';
  t.Build(s);
  Assert(t.Root.XPathNode('child').Text.Value = '<test>ici</test>');
  Assert(t.Root.XPathNode('child/test').Text.Value = 'ici');
  Assert(t.Root.XPathNode('child/test[2]').Text.Value = '2nd');
  Assert(t.Root.XPathNode('child/test[@flag]').Text.Value = 'there');
  Assert(t.Root.XPathNode('child/test[2@flag]').Text.Value = '2nd');
  Assert(t.Root.XPathNode('child/test[@flag=value]').Text.Value = 'three');
  Assert(t.Root.XPathNode('child/subchild').Text.Value = 'found');

end;



initialization
{$IFDEF DEBUG}
//  test;
{$ENDIF}
end.
