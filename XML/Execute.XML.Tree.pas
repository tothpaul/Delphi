unit Execute.XML.Tree;

{
  XML Unit for Delphi Tokyo (c)2017 by Execute SARL
  http://www.execute.fr

  v1.0 - 2017-08-12

  see Test() procedure below
}

interface

uses
  System.SysUtils;

type
  // reference to a String in the XML source
  TXMLString = record
    // pointer to the first character
    Start: PAnsiChar;
    // length of the string
    Len  : Byte;
    // search for a char in the string
    function Pos(Ch: AnsiChar): Integer;
    // compare the string with a given PAnsiChar
    function Compare(const AStr: PAnsiChar; ALen: Integer): Boolean;
    // compare the string with an other TXMLString
    function Equals(const AName: TXMLString): Boolean; inline;
    // compare the string with an UTF8String
    function Match(const AStr: UTF8String): Boolean; inline;
    // return the value of the TXMLString, safe to call on a "nil" PXMLString
    function Value: UTF8String;
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
  // name of the tag
    Name    : TXMLString;
  // attributs of the tag <tag name="value"...>
    Attrs   : TArray<TXMLAttribute>;
  // text inside the tag <tag>Text</tag>
    Text    : TXMLString;
  // children XML Node of this one
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
  // return the value of Text, safe to call on a "nil" PXMLNode
    function Value: UTF8String; inline;
  // properties
    property Child[const AName: UTF8String]: PXMLNode read GetChild; default;
    property Attribute[const AName: UTF8String]: PXMLString read GetAttribute;
  end;

  // collection of TXMLNode
  TXMLTree = record
  private
  // the XML source
    Text       : UTF8String;
  // position inside Text
    Position   : Integer;
  // readed TagName
    TagName    : TXMLString;
  // is TagName prefixed with / ?
    EndTag     : Boolean;
  // data between the opening tag and the closing one
    DataLen    : Integer;
  // skip spaces
    procedure Spaces;
  // read a TagName
    procedure GetTag;
  // read a TXMLNode
    procedure GetNode(var Node: TXMLNode);
  // read a TXMLAttribute
    procedure GetAttr(var Attr: TXMLAttribute);
  // read until Seq and return the number of characters skiped
    function ReadUntil(const Seq: UTF8String): Integer;
  // skip a sequence of chars if possible
    function Skip(const Str: UTF8String): Boolean;
  // drop a sequence of chars or raise an Exception
    procedure Drop(const Str: UTF8String);
  public
  // the Root node
    Root: TXMLNode;
  // parse AText and build the node Tree
    function Build(const AText: UTF8String): Boolean;
  // retreive a Node at the given path
    function GetNodeAt(const APath: array of string): PXMLNode;
    function HasNodeAt(const APath: array of string; var ANode: PXMLNode): Boolean;
  // limited XPath syntax
  //   //           : any node
  //   parent/child : child node
  //   child[x]     : x-nth "child" node
  //   [x]          : x-nth children
  // todo (or not todo) :
  //   child[@name]       : "child" with a "name" attribute
  //   child[@name=value] : "child" with a "name" attribute and a value "value"
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

implementation

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

function TXMLString.Value: UTF8String;
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

{ TXMLTree }

procedure TXMLTree.Spaces;
begin
  while Text[Position] in [#9, #10, #13, ' '] do Inc(Position);
end;

procedure TXMLTree.GetTag;
var
  start: Integer;
  dots : Integer;
begin
  TagName.Len := 0;
// initial start of the tag
  start := Position;
  repeat
  // reset DataLen if we have skipped comments etc...
    DataLen := Position - Start;
  // count chars until first "<"
    Inc(DataLen, ReadUntil('<'));
  // ignore <?...?> tags like <?xml version="1.0" encoding="UTF-8"?>
    if Skip('?') then
    begin
      while not Skip('?>') do
        Inc(Position);
    end else
  // ignore comments like <!-- comment -->
    if Skip('!--') then
    begin
      while not Skip('-->') do
        Inc(Position);
    end else begin
    // skip "/" in </tag>
      EndTag := Text[Position] = '/';
      if EndTag then
        Inc(Position);
    // read name
      TagName.Start := @Text[Position];
      TagName.Len := Position;
      while not (Text[Position] in [#9, #10, #13, ' ', '/', '>']) do
      begin
        Inc(Position);
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

procedure TXMLTree.GetNode(var Node: TXMLNode);
var
  Count: Integer;
  Start: Integer;
begin
  Node.Name := TagName;
// Attributes
  Spaces;
  Count := 0;
  while not (Text[Position] in ['?', '/', '>']) do
  begin
    SetLength(Node.Attrs, Count + 1);
    GetAttr(Node.Attrs[Count]);
    Inc(Count);
  end;
  Node.Text.Len := 0;
// Empty node
  if not Skip('/>') then
  begin
    Drop('>');
    Node.Text.Start := @Text[Position];
    // to compute Node.Text.Len
    Start := Position;
    Count := 0;
    GetTag;
    while (EndTag = False) or (TagName.Equals(Node.Name) = False) do
    begin
      SetLength(Node.Children, Count + 1);
      GetNode(Node.Children[Count]);
      Inc(Count);
      // adjust Text length
      Node.Text.Len := Position - Start;
      GetTag();
    end;
    // DataLen is between the last child and "</tag>"
    Inc(Node.Text.Len, DataLen);
    Drop('>');
  end;
end;

procedure TXMLTree.GetAttr(var Attr: TXMLAttribute);
var
  Quote: AnsiChar;
begin
// Attribute name
  Attr.Name.Start := @Text[Position];
  Attr.Name.Len := Position;
  while not(Text[Position] in [#9, #10, #13, ' ', '=']) do
  begin
    Inc(Position);
  end;
  Attr.Name.Len := Position - Attr.Name.Len;
// name =
  while Text[Position] <> '=' do Inc(Position);
  Inc(Position);
  Spaces;
// "value" or 'value'
  Quote := Text[Position];
  Inc(Position);
  Attr.Value.Start := @Text[Position];
  Attr.Value.Len := ReadUntil(Quote);
// prepare for next attribute
  Spaces;
end;

function TXMLTree.ReadUntil(const Seq: UTF8String): Integer;
begin
  Result := Position;
  while not Skip(Seq) do
  begin
    Inc(Position);
  end;
  Result := Position - Result - Length(Seq);
end;

function TXMLTree.Skip(const Str: UTF8String): Boolean;
var
  Len  : Integer;
  Index: Integer;
begin
  Len := Length(Str);
  for Index := 0 to Length(Str) - 1 do
  begin
    if Text[Position + Index] <> Str[Index + 1] then
      Exit(False);
  end;
  Inc(Position, Length(Str));
  Result := True;
end;

procedure TXMLTree.Drop(const Str: UTF8String);
begin
  if not Skip(Str) then
    raise Exception.Create('XML parse error : expected ' + string(Str));
end;

function TXMLTree.Build(const AText: UTF8String): Boolean;
begin
  Text := AText;
  Position := 1;
  GetTag;
  GetNode(Root);
end;

function TXMLTree.GetNodeAt(const APath: array of string): PXMLNode;
var
  Index: Integer;
begin
  if (Length(APath) = 0) or (Root.Name.Match(APath[0]) = False) then
    Exit(nil);
  Result := @Root;
  for Index := 1 to Length(APath) - 1 do
  begin
    if Result.HasChild(APath[Index], Result) = False then
      Exit;
  end;
end;

function TXMLTree.HasNodeAt(const APath: array of string; var ANode: PXMLNode): Boolean;
begin
  ANode := GetNodeAt(APath);
  Result := ANode <> nil;
end;

procedure GetName(const APath: UTF8String; var Index: Integer; var Name: UTF8STring; var Nth: Integer);
var
  Start: Integer;
begin
  Start := Index;
  Nth := -1;
  while (Index <= Length(APath)) and (APath[Index] <> '/') do
  begin
    if APath[Index] = '[' then
    begin
      Nth := 0;
      Break;
    end;
    Inc(Index);
  end;
  Name := Copy(APath, Start, Index - Start);
  if APath[Index] = '[' then
  begin
    Inc(Index);
    while APath[Index] in ['0'..'9'] do
    begin
      Nth := 10 * Nth + Ord(APath[Index]) - Ord('0');
      Inc(Index);
    end;
    if APath[Index] = ']' then
      Inc(Index);
  end;
  Inc(Index);
end;

function TXMLTree.XPathNode(const APath: UTF8String): PXMLNode;
var
  Index: Integer;
  Any  : Boolean;
  Name : UTF8String;
  Nth  : Integer;
begin
  if APath = '' then
    Exit(nil);

  Result := nil;
  Index := 1;
  if APath[1] = '/' then
    Inc(Index);

  GetName(APath, Index, Name, Nth);
  if Name = '' then
  begin
    if Nth > 0 then
      Exit;
    Any := True;
  end else begin
    if  Root.Name.Match(Name) = False then
      Exit;
    Any := False;
  end;

  Result := @Root;
  while (Result <> nil) and (Index < Length(APath)) do
  begin
    GetName(APath, Index, Name, Nth);
    if Name = '' then
    begin
      if Nth > 0 then
      begin
        if Nth > Result.Length then
          Result := nil
        else
          Result := @Result.Children[Nth - 1]
      end else
        Any := True;
    end else begin
      if Any then
      begin
        Result := Result.Search(Name, Nth);
        Any := False;
      end else begin
        Result := Result.GetNthChild(Name, Nth);
      end;
    end;
  end;
end;

function TXMLTree.XPathAttr(const APath: UTF8String): PXMLString;
var
  Index: Integer;
  Node : PXMLNode;
begin
  if APath = '' then
    Exit(nil);
  Index := Pos('@', APath);
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
var
  s: UTF8String;
  t: TXMLTree;
  n: PXMLNode;
  r: TXMLSearch;
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
end;

initialization
{$IFDEF DEBUG}
  test;
{$ENDIF}
end.

