unit DelphiTDS;

{ (c)2016 by Execute SARL, Paul TOTH <contact@execute.fr>
  http://www.execute.fr
}

// Based on:

// https://github.com/project-jedi/jcl/blob/master/jcl/source/windows/JclTD32.pas
// https://sourceforge.net/p/tds2dbg/code/HEAD/tree/trunk/tdscvstructs.h
// http://denisenkomik.narod.ru/main.cpp

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Execute.Trees;

type
// MemoryMap shows the differents items location
  TMemoryInfo = (
    mmNotRead,
    mmPadding,
    mmHeader,
    mmDirectory,
    mmDirectoryEntry,
    mmModuleInfo,
    mmSegmentInfo,
    mmSymbolInfo,
    mmSize,
    mmNameSize,
    mmName
  );

  TTDSFile = class;

  TTDSNode = class(TTreeNode)
  private
    LProc : TTDSNode;
    GProc : TTDSNode;
    Types : TTDSNode;
    Vars  : TTDSNode;
    Consts: TTDSNode;
    Proc  : TTDSNode;
  public
    Offset: Integer;
    Size  : Integer;
    procedure SetEnd(AOffset: Integer);
  end;

  TTDSFile = class
  private
    FTree   : TTreeList;
    FDump   : TTreeNode;
    FDDump  : TTreeNode;
    FSize   : Integer;
    FOffset : Integer;
    FNames  : TStringList;
    FModules: TList;
    FUDT    : TTDSNode;
    FLastMod: Integer;
    FModDump: TTDSNode;
    FModDump2: TTDSNode;
    function Get(Size: Integer; Kind: TMemoryInfo): Pointer;
    function GetI32(Kind: TMemoryInfo): Integer;
    function GetU32(Kind: TMemoryInfo): Cardinal;
    procedure GetNames(Offset: Integer);
    function GetName(Index: Integer): string;
    procedure GetModules(Offset: Integer; Root: TTDSNode);
    function GetModule(Index: Integer; Parent: TTDSNode): TTDSNode;
    procedure ReadDirectory(Offset: Integer; Root: TTDSNode);
    procedure ReadDirEntry(Offset: Integer; Parent: TTDSNode);
    procedure ReadAlignSymbols(Size: Integer; Parent: TTDSNode);
    procedure ReadAlignSymbolProc(Size: Integer; Global: Boolean; Parent: TTDSNode);
    procedure ReadAlignSymbolData(Size: Integer; Kind: Word; Parent: TTDSNode);
    procedure ReadAlignSymbolObjName(Size: Integer; Parent: TTDSNode);
    procedure ReadAlignSymbolUDT(Size: Integer; Parent: TTDSNode);
    procedure ReadAlignSymbolSSeach(Size: Integer; Parent: TTDSNode);
    procedure ReadAlignSymbolSBPRel32(Size: Integer; Parent: TTDSNode);
    procedure ReadCompiler(Size: Integer; Parent: TTDSNode);
    procedure ReadRegister(Size: Integer; Parent: TTDSNode);
    procedure ReadOptVar(Size: Integer; Parent: TTDSNode);
    procedure ReadLink(Size: Integer; Parent: TTDSNode);
    procedure ReadUses(Size: Integer; Parent: TTDSNode);
    procedure ReadNameSpaces(Size: Integer; Parent: TTDSNode);
    procedure ReadUsing(Size: Integer; Parent: TTDSNode);
    procedure ReadConst(Size: Integer; Parent: TTDSNode);
    procedure ReadSourceModules(Size: Integer; Parent: TTDSNode);
    procedure ReadGlobalTypes(Size: Integer; Parent: TTDSNode);
    function AddChild(Parent: TTreeNode; const Caption: string; Offset, Size: Integer): TTDSNode;
    function AddDump(Parent: TTreeNode; const Caption: string; Offset, Size: Integer): TTDSNode;
  public
    Memory   : array of Byte;
    MemoryMap: array of TMemoryInfo;
    constructor Create(const AFileName: string; Tree: TTreeList);
    destructor Destroy; override;
    function KnownSize: Integer;
    property Size: Integer read FSize;
  end;

implementation

function GetKind(Kind: Integer): string;
begin
  case Kind of
    $0074: Exit('Integer');
  else
    Result := '$' + IntToHex(Kind, 4);
  end;
end;

const
  SIGN : array[0..3] of AnsiChar = 'FB09';

{
   :Start
   "FB09"
   @Directory
   ...
   :Directory
    DirEntry
   "FB09"
   @Start
}

  SUBSECTION_TYPE_MODULE         = $120; //
  SUBSECTION_TYPE_TYPES          = $121;
  SUBSECTION_TYPE_SYMBOLS        = $124;
  SUBSECTION_TYPE_ALIGN_SYMBOLS  = $125; //
  SUBSECTION_TYPE_SOURCE_MODULE  = $127; //
  SUBSECTION_TYPE_GLOBAL_SYMBOLS = $129; //
  SUBSECTION_TYPE_GLOBAL_TYPES   = $12B; //
  SUBSECTION_TYPE_NAMES          = $130; //

  { Symbol type defines }
  SYMBOL_TYPE_COMPILE        = $0001; // Compile flags symbol
  SYMBOL_TYPE_REGISTER       = $0002; // Register variable
  SYMBOL_TYPE_CONST          = $0003; // Constant symbol
  SYMBOL_TYPE_UDT            = $0004; // User-defined Type
  SYMBOL_TYPE_SSEARCH        = $0005; // Start search
  SYMBOL_TYPE_END            = $0006; // End block, procedure, with, or thunk
  SYMBOL_TYPE_SKIP           = $0007; // Skip - Reserve symbol space
  SYMBOL_TYPE_CVRESERVE      = $0008; // Reserved for Code View internal use
  SYMBOL_TYPE_OBJNAME        = $0009; // Specify name of object file

  SYMBOL_TYPE_USES           = $0024;
  SYMBOL_TYPE_USING          = $0026;
  SYMBOL_TYPE_CONSTANT       = $0027;

  SYMBOL_TYPE_BPREL16        = $0100; // BP relative 16:16
  SYMBOL_TYPE_LDATA16        = $0101; // Local data 16:16
  SYMBOL_TYPE_GDATA16        = $0102; // Global data 16:16
  SYMBOL_TYPE_PUB16          = $0103; // Public symbol 16:16
  SYMBOL_TYPE_LPROC16        = $0104; // Local procedure start 16:16
  SYMBOL_TYPE_GPROC16        = $0105; // Global procedure start 16:16
  SYMBOL_TYPE_THUNK16        = $0106; // Thunk start 16:16
  SYMBOL_TYPE_BLOCK16        = $0107; // Block start 16:16
  SYMBOL_TYPE_WITH16         = $0108; // With start 16:16
  SYMBOL_TYPE_LABEL16        = $0109; // Code label 16:16
  SYMBOL_TYPE_CEXMODEL16     = $010A; // Change execution model 16:16
  SYMBOL_TYPE_VFTPATH16      = $010B; // Virtual function table path descriptor 16:16

  SYMBOL_TYPE_BPREL32        = $0200; // BP relative 16:32
  SYMBOL_TYPE_LDATA32        = $0201; // Local data 16:32
  SYMBOL_TYPE_GDATA32        = $0202; // Global data 16:32
  SYMBOL_TYPE_PUB32          = $0203; // Public symbol 16:32
  SYMBOL_TYPE_LPROC32        = $0204; // Local procedure start 16:32
  SYMBOL_TYPE_GPROC32        = $0205; // Global procedure start 16:32
  SYMBOL_TYPE_THUNK32        = $0206; // Thunk start 16:32
  SYMBOL_TYPE_BLOCK32        = $0207; // Block start 16:32
  SYMBOL_TYPE_WITH32         = $0208; // With start 16:32
  SYMBOL_TYPE_LABEL32        = $0209; // Label 16:32
  SYMBOL_TYPE_CEXMODEL32     = $020A; // Change execution model 16:32
  SYMBOL_TYPE_VFTPATH32      = $020B; // Virtual function table path descriptor 16:32
  SYMBOL_TYPE_OPTVAR32       = $0211;
  SYMBOL_TYPE_SLINK          = $0230;

type
  THeader = record
    Sign : Cardinal;
    Size : Cardinal;
  end;

  TDirectory = packed record
    Size       : Word;     // SizeOf(TDirectory)
    EntrySize  : Word;     // SizeOf(TDirEntry)
    EntryCount : Integer;  // # of TDirEntry
    NextDir    : Cardinal; // unused
    Flags      : Cardinal; // unused
  end;

  TDirEntry = packed record
    SectionType: Word;
    ModuleIndex: Word;     //
    Offset     : Cardinal;
    Size       : Cardinal;
  end;

  TSegmentInfo = packed record
    Segment: Word;     // Segment that this structure describes
    Flags  : Word;     // Attributes for the logical segment.
                       // The following attributes are defined:
                       //   $0000  Data segment
                       //   $0001  Code segment
    Offset : Cardinal; // Offset in segment where the code starts
    Size   : Cardinal; // Count of the number of bytes of code in the segment
  end;

  TModuleInfo = packed record
    OverlayNumber : Word; // Overlay number
    LibraryIndex  : Word; // Index into sstLibraries subsection
                          // if this module was linked from a library
    SegmentCount  : Word; // Count of the number of code segments
                          // this module contributes to
    DebuggingStyle: Word; // Debugging style  for this  module.
    NameIndex     : Cardinal;     // Name index of module.
    TimeStamp     : Cardinal;     // Time stamp from the OBJ file.
    Reserved      : array [0..2] of Cardinal; // Set to 0.
//    Segments      : array [0..0] of TSegmentInfo;
                          // Detailed information about each segment
                          // that code is contributed to.
                          // This is an array of cSeg count segment
                          // information descriptor structures.
  end;

  TSymbolProcInfo = packed record
    pParent: Cardinal;
    pEnd: Cardinal;
    pNext: Cardinal;
    Size: Cardinal;      // Length in bytes of this procedure
    DebugStart: Cardinal;// Offset in bytes from the start of the procedure to
                         // the point where the stack frame has been set up.
    DebugEnd: Cardinal;  // Offset in bytes from the start of the procedure to
                         // the point where the  procedure is  ready to  return
                         // and has calculated its return value, if any.
                         // Frame and register variables an still be viewed.
    Offset: Cardinal;    // Offset portion of  the segmented address of
                         // the start of the procedure in the code segment
    Segment: Word;       // Segment portion of the segmented address of
                         // the start of the procedure in the code segment
    ProcType: Cardinal;  // Type of the procedure type record
    NearFar: Byte;       // Type of return the procedure makes:
                         //   0       near
                         //   4       far
    Reserved: Byte;
    NameIndex: Cardinal; // Name index of procedure
    Unknown  : Cardinal;
  end;

  TSymbolObjNameInfo = packed record
    Signature: Cardinal; // Signature for the CodeView information contained in
                         // this module
    NameIndex: Cardinal; // Name index of the object file
  end;

  TSymbolDataInfo = packed record
    Offset: Cardinal;    // Offset portion of  the segmented address of
                         // the start of the data in the code segment
    Segment: Word;       // Segment portion of the segmented address of
                         // the start of the data in the code segment
    Reserved: Word;
    TypeIndex: Cardinal; // Type index of the symbol
    NameIndex: Cardinal; // Name index of the symbol
    Unknown  : Cardinal;
  end;

  TSymbolWithInfo = packed record
    pParent: Cardinal;
    pEnd: Cardinal;
    Size: Cardinal;      // Length in bytes of this "with"
    Offset: Cardinal;    // Offset portion of the segmented address of
                         // the start of the "with" in the code segment
    Segment: Word;       // Segment portion of the segmented address of
                         // the start of the "with" in the code segment
    Reserved: Word;
    NameIndex: Cardinal; // Name index of the "with"
  end;

  TSymbolLabelInfo = packed record
    Offset: Cardinal;    // Offset portion of  the segmented address of
                         // the start of the label in the code segment
    Segment: Word;       // Segment portion of the segmented address of
                         // the start of the label in the code segment
    NearFar: Byte;       // Address mode of the label:
                         //   0       near
                         //   4       far
    Reserved: Byte;
    NameIndex: Cardinal; // Name index of the label
  end;

  TSymbolConstantInfo = packed record
    TypeIndex: Cardinal;   // Type index of the constant (for enums)
    NameIndex: Cardinal;   // Name index of the constant
    Reserved: Cardinal;
    Value: Cardinal;       // value of the constant
  end;

  TSymbolUdtInfo = packed record
    TypeIndex: Cardinal; // Type index of the type
    Properties: Word;    // isTag:1 True if this is a tag (not a typedef)
                         // isNest:1 True if the type is a nested type (its name
                         // will be 'class_name::type_name' in that case)
    NameIndex: Cardinal; // Name index of the type
    Reserved: Cardinal;
  end;

  TSymbolSSearchInfo = packed record
    Offset     : Cardinal;
    Segment    : Word;
    CodeSymbols: Word;
    DataSymbols: Word;
    FirstData  : Integer;
    Unknown    : Word;
  end;

  TSBPRel32 = packed record
    EbpOffset: Integer;
    Kind     : Word;
    Unknown1 : Word;
    Name     : Cardinal;
    Unknown2 : Cardinal;
  end;

  TSymbolVftPathInfo = packed record
    Offset: Cardinal;    // Offset portion of start of the virtual function table
    Segment: Word;       // Segment portion of the virtual function table
    Reserved: Word;
    RootIndex: Cardinal; // The type index of the class at the root of the path
    PathIndex: Cardinal; // Type index of the record describing the base class
                         // path from the root to the leaf class for the virtual
                         // function table
  end;

  TSymbolConstInfo = packed record
    Kind : Integer;
    Prop : Word;
    Name : Integer;
    BrowserOffset: Integer;
    Value: Cardinal; // ?
  end;

  TSymbolInfo = packed record
    Size: Word;
    Kind: Word;
  end;
//    case SymbolType: Word of
//      SYMBOL_TYPE_LPROC32,
//      SYMBOL_TYPE_GPROC32:
//        (Proc: TSymbolProcInfo);
//      SYMBOL_TYPE_OBJNAME:
//        (ObjName: TSymbolObjNameInfo);
//      SYMBOL_TYPE_LDATA32,
//      SYMBOL_TYPE_GDATA32,
//      SYMBOL_TYPE_PUB32:
//        (Data: TSymbolDataInfo);
//      SYMBOL_TYPE_WITH32:
//        (With32: TSymbolWithInfo);
//      SYMBOL_TYPE_LABEL32:
//        (Label32: TSymbolLabelInfo);
//      SYMBOL_TYPE_CONST:
//        (Constant: TSymbolConstantInfo);
//      SYMBOL_TYPE_UDT:
//        (Udt: TSymbolUdtInfo);
//      SYMBOL_TYPE_VFTPATH32:
//        (VftPath: TSymbolVftPathInfo);
//  end;

  TSymbolInfos = packed record
    Signature: Cardinal;
//    Symbols: array [0..0] of TSymbolInfo;
  end;

  TSourceModuleInfo = packed record
    FileCount: Word;    // The number of source file scontributing code to segments
    SegmentCount: Word; // The number of code segments receiving code from this module
  // array [FileCount] of @TSourceFileEntry
  // array [SegmentCount] of record Start, Stop: Cardinal end
  // array [SegmentCount] of Word
  end;

  TSourceFileEntry = packed record
    LineCount: Word; // Number of segments that receive code from this source file.
    NameIndex: Cardinal;   // Name index of Source file name.
  // array [LineCount] of @LineInfo
  // array [LineCount] of record Start, Stop: Cardinal end
  end;

  TSourceLineInfo = packed record
    Segment  : Word;
    LineCount: Word;
  // array[LineCount] of record Start, Stop: Cardinal end
  // array[LineCount] of Word
  end;

  TCompilerInfo = packed record
    Machine     : Byte;
    Language    : Byte;
    Unkwnow     : Word;
    CompNameLen : Byte;
    // CompName
  end;

  TRegisterInfo = packed record
    Kind       : Integer;
    Register   : Word;
    Name       : Integer;
    BrowserOfs : Integer;
  end;

  TOptVarRegInfo = packed record
    Start   : Integer;
    Size    : Integer;
    Register: Word;
  end;

  TOptVarInfo = packed record
    Num: Word;
    // Items: array[num] of TOptVarRegInfo
  end;

  TArrayOfCardinals = array[Word] of Cardinal;
  TArrayOfCardinalPairs = array[Word] of record Start, Stop: Cardinal end;
  TArrayOfWords = array[Word] of Word;

{ TTDSFile }

function TTDSFile.AddChild(Parent: TTreeNode; const Caption: string;
  Offset, Size: Integer): TTDSNode;
begin
  Result := TTDSNode.Create(FTree, Parent, Caption);
  Result.Offset := Offset;
  Result.Size := Size;
end;

function TTDSFile.AddDump(Parent: TTreeNode; const Caption: string; Offset,
  Size: Integer): TTDSNode;
begin
  Result := AddChild(Parent, IntToHex(Offset, 8) + '-' + IntToHex(Offset + Size - 1, 8) + ' ' + Caption, Offset, Size);
end;

constructor TTDSFile.Create(const AFileName: string; Tree: TTreeList);
var
  Stream: TFileStream;
  Size  : Integer;
  Header: THeader;
  Start : Integer;
  Root  : TTDSNode;
  Node  : TTreeNode;
  Next  : TTreeNode;
begin
  FTree := Tree;
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
  // Check for a TDS file (works also for an Exe file !)
    Size := Stream.Size;
    if Size < 4 * SizeOf(Cardinal) then
      raise Exception.Create('Too small for a TDS file');

    Stream.Position := Size - SizeOf(Header);
    Stream.Read(Header, SizeOf(Header));

    if Header.Sign <> Cardinal(SIGN) then
      raise Exception.Create('Not a TDS file');

    FSize := Header.Size;
    Start := Size - FSize;
    Stream.Position := Start;

    SetLength(Memory, FSize);
    Stream.Read(Memory[0], FSize);
    SetLength(MemoryMap, FSize);
  finally
    Stream.Free;
  end;

  if GetU32(mmHeader) <> Cardinal(SIGN) then
    raise Exception.Create('Corrupted TDS file');

  Start := GetU32(mmHeader); // Directory Offset

  FNames := TStringList.Create;
  GetNames(Start); // Extract the Strings DirEntry

  Tree.BeginUpdate;
  try
    Root := TTDSNode.Create(FTree, nil, 'TD32');
    FDump := TTDSNode.Create(FTree, nil, 'DUMP');
    FDump.Sorted := True;
    AddDump(FDump, 'SIGN', 0, 4);
    AddDump(FDump, 'SIZE', 4, 4);
    AddDump(FDump, 'SIGN', FSize - 8, 4);
    AddDump(FDump, 'SIZE', FSize - 4, 4);

    FModules := TList.Create;
    GetModules(Start, Root);

    FLastMod := -1;
    ReadDirectory(Start, Root);

    Node := FDump.FirstChild;
    repeat
      Next := Node;
      if FDump.NextChild(Next) then
      begin
        Size := TTDSNode(Next).Offset - (TTDSNode(Node).Offset + TTDSNode(Node).Size);
        if Size > 0 then
        begin
          AddDump(FDump, ' *** PADDING ' + IntToStr(Size), TTDSNode(Node).Offset + TTDSNode(Node).Size, Size);
        end else
        if Size < 0 then
        begin
          AddDump(FDump, ' *** OVERFLOW ' + IntToStr(- Size), TTDSNode(Next).Offset, -Size);
          Node := Next;
        end;
      end;
    until FDump.NextChild(Node) = False;
  finally
    Tree.EndUpdate;
  end;
end;

destructor TTDSFile.Destroy;
begin
  FNames.Free;
  FModules.Free;
  inherited;
end;

function TTDSFile.Get(Size: Integer; Kind: TMemoryInfo): Pointer;
begin
  Result := @Memory[FOffset];
  FillChar(MemoryMap[FOffset], Size, Kind);
  Inc(FOffset, Size);
end;

function TTDSFile.GetI32(Kind: TMemoryInfo): Integer;
begin
  Result := Integer(Get(SizeOf(Result), Kind)^);
end;

function TTDSFile.GetModule(Index: Integer; Parent: TTDSNode): TTDSNode;
begin
  if (Index < 0) or (Index >= FModules.Count) then
    Result := AddChild(Parent.Parent, 'Module#' + IntToStr(Index), FOffset, 0)
  else
    Result := FModules[Index];
end;

procedure TTDSFile.GetModules(Offset: Integer; Root: TTDSNode);
var
  Start  : Integer;
  Dir    : ^TDirectory;
  Index  : Integer;
  Ofs    : Integer;
  Entry  : ^TDirEntry;
  Module : ^TModuleInfo;
  Count  : Integer;
  sIndex : Integer;
  Segment: ^TSegmentInfo;
  Str    : string;
  Node   : TTDSNode;
  ModDump: TTDSNode;
  CodeSize: Integer;
  DataSize: Integer;
begin
  Root := AddChild(Root, 'Modules', 0, Size);
  Root.Sorted := True;

  FModules.Add(AddChild(Root, '(Global)', 0, 0));

  FOffset := Offset;
  Dir := Get(SizeOf(TDirectory), mmDirectory);

  if Dir.Size <> SizeOf(TDirectory) then
    raise Exception.Create('Invalid Directory Size');

  if Dir.EntrySize <> SizeOf(TDirEntry) then
    raise Exception.Create('Invalid Directory Entries Size');

  FDDump := nil;

  for Index := 0 to Dir.EntryCount - 1 do
  begin

    Start := FOffset;
    Entry := Get(SizeOf(TDirEntry), mmDirectoryEntry);

    if Entry.SectionType = SUBSECTION_TYPE_MODULE then // Module
    begin

      Assert(Entry.Size = 76); // it's not a valide size !!!

      if FDDump = nil then
      begin
        FDDump := AddDump(FDump, 'Modules [$0120]', Entry.Offset, 0);
      end;

      Ofs := FOffset;

      FOffset := Entry.Offset;
      Module := Get(SizeOf(TModuleInfo), mmModuleInfo);

      Assert(Module.OverlayNumber = 0, 'OverLayNumber <> 0');
      Assert(Module.LibraryIndex = 0, 'LivrayIndex <> 0');
      Assert(Module.DebuggingStyle = $4356, 'DebbingStyle <> $4356'); // "CV"

      TTDSNode(FDDump).SetEnd(Entry.Offset + SizeOf(TModuleinfo) + Module.SegmentCount * SizeOf(TSegmentInfo));
      ModDump := AddDump(FDDump, GetName(Module.NameIndex), Entry.Offset, SizeOf(TModuleinfo) + Module.SegmentCount * SizeOf(TSegmentInfo));//Entry.Size);

      Node := AddChild(Root, GetName(Module.NameIndex), Entry.Offset, SizeOf(TModuleinfo) + Module.SegmentCount * SizeOf(TSegmentInfo));//Entry.Size);
      FModules.Add(Node);
      Count := Module.SegmentCount;
      CodeSize := 0;
      DataSize := 0;
      if Count > 0 then
      begin
        Node := AddChild(Node, 'Segments', FOffset, Count * SizeOf(TSegmentInfo));
        for sIndex := 0 to Count - 1 do
        begin
          Segment := Get(SizeOf(TSegmentInfo), mmSegmentInfo);
          Str := '#' + IntToStr(Segment.Segment);
          if Segment.Flags = 1  then
          begin
            Str := Str + ' CODE ';
            Inc(CodeSize, Segment.Size);
          end else begin
            str := Str + ' DATA ';
            Inc(DataSize, Segment.Size);
          end;
          Str := Str + IntToHex(Segment.Offset, 8) + '-' + IntToHex(Segment.Offset + Segment.Size, 8);
          AddChild(Node, Str, FOffset - SizeOf(TSegmentInfo), SizeOf(TSegmentInfo));
          AddDump(ModDump, Str, FOffset - SizeOf(TSegmentInfo), SizeOf(TSegmentInfo));
        end;
      end;
      AddChild(Node, 'CodeSize = ' + IntToStr(CodeSize), 0, 0);
      AddChild(Node, 'DataSize = ' + IntToStr(DataSize), 0, 0);

      FOffset := Ofs;
    end;
  end;
end;

function TTDSFile.GetU32(Kind: TMemoryInfo): Cardinal;
begin
  Result := Cardinal(Get(SizeOf(Cardinal), Kind)^);
end;

function TTDSFile.KnownSize: Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to Size - 1 do
    if MemoryMap[Index] <> mmNotRead then
      Inc(Result);
end;

function TTDSFile.GetName(Index: Integer): string;
begin
  if (Index < 0) or (Index >= FNames.Count) then
    Result := 'no_name'
  else
    Result := FNames[Index];
end;

procedure TTDSFile.GetNames(Offset: Integer);
var
  Dir   : ^TDirectory;
  Index : Integer;
  Entry : ^TDirEntry;
  Count : Integer;
  sIndex: Integer;
  Len   : Byte;
  Name  : string;
  PStr  : PAnsiChar;
begin
  FNames.Add('#NO_NAME'); // 0 = no name
  FOffset := Offset;
  Dir := Get(SizeOf(TDirectory), mmDirectory);

  if Dir.Size <> SizeOf(TDirectory) then
    raise Exception.Create('Invalid Directory Size');

  if Dir.EntrySize <> SizeOf(TDirEntry) then
    raise Exception.Create('Invalid Directory Entries Size');

  for Index := 0 to Dir.EntryCount - 1 do
  begin
    Entry := Get(SizeOf(TDirEntry), mmDirectoryEntry);
    if Entry.SectionType = $130 then
    begin
    {
       Count: Integer : number of strings
       Len0: Byte; PAnsiChar0: Array of AnsiChar
       Len1: Byte; PAnsiChar1: Array of AnsiChar
       ...
    }
      FOffset := Entry.Offset;
      Count := PInteger(Get(SizeOf(Integer), mmSize))^;
      for sIndex := 0 to Count - 1 do
      begin
        Len := PByte(Get(SizeOf(Byte), mmNameSize))^;
        PStr := Get(Len, mmName);
        Name := PStr; // PChar -> String ... can we have a more than 255 char string ?
        if Length(Name) < Len then // the String contains #0, trust Len
          SetString(Name, PStr, Len);
        Get(Length(Name) - Len + 1, mmName);
        FNames.Add(Name);
      end;
      Exit;
    end;
  end;
end;


procedure TTDSFile.ReadDirectory(Offset: Integer; Root: TTDSNode);
var
  Dir  : ^TDirectory;
  Index: Integer;
  Ofs  : Integer;
begin
  FOffset := Offset;
  Dir := Get(SizeOf(TDirectory), mmDirectory);
  Assert(Dir.NextDir = 0);
  Assert(Dir.Flags = 0);
  if Dir.Size <> SizeOf(TDirectory) then
    raise Exception.Create('Invalid Directory Size');
  if Dir.EntrySize <> SizeOf(TDirEntry) then
    raise Exception.Create('Invalid Directory Entries Size');
  FDDump := AddDump(FDump, 'Directory', Offset, SizeOf(TDirectory) + Dir.EntryCount * Dir.EntrySize);
  Ofs := FOffset;
  FModDump := nil;
  for Index := 0 to Dir.EntryCount - 1 do
  begin
    ReadDirEntry(Ofs, Root);
    Inc(Ofs, Dir.EntrySize);
  end;
end;

procedure TTDSFile.ReadDirEntry(Offset: Integer; Parent: TTDSNode);
var
  Entry  : ^TDirEntry;
  Detail : TTDSNode;
begin
  FOffset := Offset;

  Entry := Get(SizeOf(TDirEntry), mmDirectoryEntry);

  Parent := GetModule(Entry.ModuleIndex, Parent);

  if Entry.SectionType = SUBSECTION_TYPE_MODULE then
  begin
    if FModDump = nil then
      FModDump := AddDump(FDDump, 'Modules', Offset, FModules.Count * SizeOf(TDirEntry));
    Detail := AddDump(FModDump, Parent.Text, Offset, SizeOf(TDirEntry));
    AddDump(Detail, 'Size = ' + IntToStr(Entry.Size), Offset + 8, 4);
    Exit;
  end;

  Detail := AddDump(FDDump, 'DirEnry $' + IntToHex(Entry.SectionType, 4) + ' for ' + Parent.Text, Offset, SizeOf(TDirEntry));
  AddDump(Detail, 'Size = ' + IntToStr(Entry.Size), Offset + 8, 4);

  if Entry.ModuleIndex <> FLastMod then
  begin
    FLastMod := Entry.ModuleIndex;
    FModDump := AddDump(FDump, Parent.Text, Entry.Offset, 0);
  end;
  FModDump.SetEnd(Entry.Offset + Entry.Size);

//  Parent := AddChild(Parent, 'EntryModule#' + IntToStr(Entry.ModuleIndex), FOffset);
  FOffset := Entry.Offset;
  case Entry.SectionType of
    SUBSECTION_TYPE_ALIGN_SYMBOLS:
    begin
      AddDump(FModDump, 'Symbols [$0125] for ' + Parent.Text, Entry.Offset, Entry.Size);
      ReadAlignSymbols(Entry.Size, Parent);
    end;
    SUBSECTION_TYPE_SOURCE_MODULE:
    begin
      FModDump2 := AddDump(FModDump, 'Source [$0127] for ' + Parent.Text, Entry.Offset, Entry.Size);
      ReadSourceModules(Entry.Size, Parent);
    end;
    SUBSECTION_TYPE_NAMES:
    begin
      AddDump(FModDump, 'Strings [$0130]', Entry.Offset, Entry.Size);
    end;
//      ReadNames(Entry.Size, Parent);
    SUBSECTION_TYPE_GLOBAL_TYPES:
    begin
      AddDump(FModDump, 'GlobalType [$012B]', Entry.Offset, Entry.Size);
      ReadGlobalTypes(Entry.Size, Parent);
    end;
    SUBSECTION_TYPE_GLOBAL_SYMBOLS:
    begin
      AddDump(FModDump, 'GlobalSymbols [$0129]', Entry.Offset, Entry.Size);
    end;
  else
    AddDump(FModDump, 'DirEntry [$' + IntToHex(Entry.SectionType, 4) + '] for ' + Parent.Text, Entry.Offset, Entry.Size);
//    raise Exception.Create('Unknown Directory Entry $' + IntToHex(Entry.SectionType, 4));
  end;
end;

procedure TTDSFile.ReadAlignSymbolData(Size: Integer; Kind: Word;
  Parent: TTDSNode);
var
  Info: ^TSymbolDataInfo;
begin
  if Parent.Vars = nil then
    Parent.Vars := AddChild(Parent, 'Vars', FOffset, 0);
//  Parent := Parent.Data;
  Info := Get(SizeOf(TSymbolDataInfo), mmSymbolInfo);
  Parent := AddChild(Parent.Vars, GetName(Info.NameIndex) + ' Kind#' + IntToStr(Kind) + ' Type#' + IntToStr(Info.TypeIndex) + ' Seg#' + IntToStr(Info.Segment), FOffset - SizeOf(TSymbolDataInfo), SizeOf(TSymbolDataInfo));
  AddChild(Parent, 'Kind = ' + GetKind(Kind), 0, 0);
  AddChild(Parent, 'Type = ' + GetName(Info.TypeIndex), 0, 0);
end;

procedure TTDSFile.ReadAlignSymbolObjName(Size: Integer; Parent: TTDSNode);
var
  Info: ^TSymbolObjNameInfo;
begin
  Assert(Size >= SizeOf(TSymbolObjNameInfo));
  Info := Get(SizeOf(TSymbolObjNameInfo), mmSymbolInfo);
  AddChild(Parent, GetName(Info.NameIndex) + ' #' + IntToStr(Info.Signature), FOffset, SizeOf(TSymbolObjNameInfo));
end;

procedure TTDSFile.ReadAlignSymbolProc(Size: Integer; Global: Boolean;
  Parent: TTDSNode);
var
  Proc: TTDSNode;
  Info: ^TSymbolProcInfo;
begin
  if Global then
  begin
    if Parent.GProc = nil then
      Parent.GProc := AddChild(Parent, 'Global Procs', FOffset, Size);
    Proc := Parent.GProc;
  end else begin
    if Parent.LProc = nil then
      Parent.LProc := AddChild(Parent, 'Local Procs', FOffset, Size);
    Proc := Parent.LProc;
  end;
  Assert(Size >= SizeOf(TSymbolProcInfo));
  Info := Get(SizeOf(TSymbolProcInfo), mmSymbolInfo);
  Parent.Proc := AddChild(Proc, GetName(Info.NameIndex), FOffset - SizeOf(TSymbolProcInfo) - SizeOf(TSymbolInfo), SizeOf(TSymbolProcInfo) + SizeOf(TSymbolInfo));
end;

procedure TTDSFile.ReadAlignSymbols(Size: Integer; Parent: TTDSNode);
var
  Info: ^TSymbolInfo;
  EOD : Integer;
  Ofs : Integer;
begin
  EOD := FOffset + Size;
  if GetU32(mmSymbolInfo) <> 1 then
    raise Exception.Create('wrong AlignSymbols signature');
//  Parent := AddChild(Parent, 'Symbols', EOD - Size, 0);
  while FOffset < EOD do
  begin
    Ofs := FOffset;
    Info := Get(SizeOf(TSymbolInfo), mmSymbolInfo);

    case Info.Kind of
      SYMBOL_TYPE_COMPILE : ReadCompiler(Info.Size, Parent);
      SYMBOL_TYPE_REGISTER: ReadRegister(Info.Size, Parent);
      SYMBOL_TYPE_OBJNAME : ReadAlignSymbolObjName(Info.Size, Parent);
      SYMBOL_TYPE_LPROC32, // Local
      SYMBOL_TYPE_GPROC32: // Global
             ReadAlignSymbolProc(Info.Size, Info.Kind = SYMBOL_TYPE_GPROC32, Parent);
      SYMBOL_TYPE_LDATA32, // Local
      SYMBOL_TYPE_GDATA32, // Global
      SYMBOL_TYPE_PUB32: // Public
             ReadAlignSymbolData(Info.Size, Info.Kind, Parent);
//      SYMBOL_TYPE_WITH32:
//        (With32: TSymbolWithInfo);
//        AddChild(Parent, 'With', FOffset, SizeOf(TSymbolInfo));
      SYMBOL_TYPE_LABEL32:
//        (Label32: TSymbolLabelInfo);
        AddChild(Parent, 'Label', FOffset, SizeOf(TSymbolInfo));
      SYMBOL_TYPE_CONST:
//        (Constant: TSymbolConstantInfo);
        AddChild(Parent, 'Const', FOffset, SizeOf(TSymbolInfo));
      SYMBOL_TYPE_UDT:
//        (Udt: TSymbolUdtInfo);
//        AddChild(Parent, 'UDT', FOffset);
        ReadAlignSymbolUDT(Info.Size, Parent);
      SYMBOL_TYPE_VFTPATH32:
//        (VftPath: TSymbolVftPathInfo);
        AddChild(Parent, 'ftPath', FOffset, SizeOf(TSymbolInfo));
      SYMBOL_TYPE_SSEARCH:
        ReadAlignSymbolSSeach(Info.Size, Parent);
      SYMBOL_TYPE_END:
      begin
        Assert(Parent.Proc <> nil);
        AddChild(Parent.Proc, 'END', Ofs, SizeOf(TSymbolInfo));
        Parent.Proc := nil;
      end;
      SYMBOL_TYPE_USES: ReadUses(Info.Size, Parent);
//      $0025: ReadNameSpaces(Info.Size, Parent);
      SYMBOL_TYPE_USING: ReadUsing(Info.Size, Parent);
      SYMBOL_TYPE_CONSTANT: ReadConst(Info.Size, Parent);
      SYMBOL_TYPE_BPREL32: ReadAlignSymbolSBPRel32(Info.Size, Parent);
      SYMBOL_TYPE_OPTVAR32: ReadOptVar(Info.Size, Parent);
      SYMBOL_TYPe_SLINK   : ReadLink(Info.Size, Parent);
    else
      AddChild(Parent, 'ERROR $' + IntToHex(Info.Kind, 4), Ofs, SizeOf(Info.Size) + Info.Size);
      Get(Info.Size - SizeOf(Info.Kind), mmNotRead);
    end;
    Inc(Ofs, SizeOf(Info.Size) + Info.Size);
    if FOFfset < Ofs then
    begin
      AddChild(Parent, 'PADDING (' + IntToHex(Info.Kind, 4) + ')' + IntToStr(Ofs - FOffset) + ' bytes', FOffset, Ofs - FOffset);
      Get(Ofs - FOffset, mmPadding);
    end;
    if FOffset > Ofs then
      raise Exception.Create('Read Overflow');
    FOffset := Ofs;
  end;
  Assert(FOffset = EOD, 'Size is not null'); // ?!
end;

procedure TTDSFile.ReadAlignSymbolSBPRel32(Size: Integer; Parent: TTDSNode);
var
  Start: Integer;
  Info : ^TSBPRel32;
begin
  if Parent.Proc <> nil then
    Parent := Parent.Proc;
  Start := FOffset;
  Info := Get(SizeOf(TSBPRel32), mmSymbolInfo);
  Parent := AddChild(Parent, GetName(Info.Name), Start - SizeOf(TSymbolInfo), SizeOf(TSBPRel32) + SizeOf(TSymbolInfo));
  AddChild(Parent, 'EbpOffset = ' + IntToStr(Info.EbpOffset), Start, 4);
  AddChild(Parent, 'Kind = ' + GetKind(Info.Kind), Start + 4, 2);
  AddChild(Parent, 'Unknown1 = ' + IntToStr(Info.Unknown1), Start + 4 + 2, 2);
//  AddChild(Parent, 'Name = ' + GetName(Info.Name), Start + 4 + 2 + 2, 4);
  AddChild(Parent, 'Unknown2 = ' + IntToStr(Info.Unknown2), Start + 4 + 2 + 2 + 4, 4);
end;

procedure TTDSFile.ReadAlignSymbolSSeach(Size: Integer; Parent: TTDSNode);
var
  Start: Integer;
  Info: ^TSymbolSSearchInfo;
begin
  Start := FOffset;
  Info := Get(SizeOf(TSymbolSSearchInfo), mmSymbolInfo);
  Parent := AddChild(Parent, 'SSearch', Start - SizeOf(TSymbolInfo), SizeOf(TSymbolSSearchInfo) + SizeOf(TSymbolInfo));
  AddChild(Parent, 'Offset = ' + IntToStr(Info.Offset), Start, 4);
  AddChild(Parent, 'Segment = ' + IntToStr(Info.Segment), Start + 4, 2);
  AddChild(Parent, 'CodeSymbols = ' + IntToStr(Info.CodeSymbols), Start + 4 + 2, 2);
  AddChild(Parent, 'DataSymbols = ' + IntToStr(Info.DataSymbols), Start + 4 + 2 + 2, 2);
  AddChild(Parent, 'FirstData = ' + IntToStr(Info.FirstData), Start + 4 + 2 + 2 + 2, 4);
  AddChild(Parent, 'Unknown = ' + IntToStr(Info.Unknown), Start + 4 + 2 + 2 + 2 + 4, 2);
end;

procedure TTDSFile.ReadAlignSymbolUDT(Size: Integer; Parent: TTDSNode);
var
  Info: ^TSymbolUdtInfo;
begin
  if Parent.Types = nil then
    Parent.Types := AddChild(Parent, 'Types', FOffset, 0);
//  Parent := Parent.UDT;
  Info := Get(SizeOf(TSymbolUdtInfo), mmSymbolInfo);
  AddChild(Parent.Types, GetName(Info.NameIndex) + ' (' + IntToStr(Info.TypeIndex) + ')', FOffset - SizeOf(TSymbolUdtInfo), SizeOf(TSymbolUdtInfo));
end;

procedure TTDSFile.ReadCompiler(Size: Integer; Parent: TTDSNode);
var
  Start: Integer;
  Info : ^TCompilerInfo;
  PStr : PAnsiChar;
  Name : string;
begin
  Start := FOffset;
  Info := Get(SizeOf(TCompilerInfo), mmSymbolInfo);
  PStr := Get(Info.CompNameLen, mmSymbolInfo);
  SetString(Name, PStr, Info.CompNameLen);
  Parent := AddChild(Parent, 'Compiler', Start - SizeOf(TSymbolInfo), SizeOf(TCompilerInfo) + SizeOf(TSymbolInfo) + Info.CompNameLen);
  AddChild(Parent, 'Machine = ' + IntToStr(Info.Machine), Start, 1);
  AddChild(Parent, 'Language = ' + IntToStr(Info.Language), Start + 1, 1);
  AddChild(Parent, 'Unknown = ' + IntToStr(Info.Unkwnow), Start + 2, 2);
  AddChild(Parent, 'Name = ' + Name, Start + 4, Length(Name) + 1);
end;

procedure TTDSFile.ReadConst(Size: Integer; Parent: TTDSNode);
var
  Start: Integer;
  Info : ^TSymbolConstInfo;
  Index: Integer;
begin
  if Parent.Consts = nil then
    Parent.Consts := AddChild(Parent, 'Consts', FOffset, Size);
  Start := FOffset;
  Info := Get(SizeOf(TSymbolConstInfo), mmSymbolInfo);
  Parent := AddChild(Parent.Consts, GetName(Info.Name), Start - SizeOf(TSymbolInfo), SizeOf(TSymbolConstInfo) + SizeOf(TSymbolInfo));
  AddChild(Parent, 'Kind = ' + GetKind(Info.Kind), Start, 4);
  AddChild(Parent, 'Prop = ' + IntToStr(Info.Prop), Start + 4 , 2);
  AddChild(Parent, 'Ofs = ' + IntToStr(Info.BrowserOffset), Start + 4 + 4 , 4);
  AddChild(Parent, 'Value = ' + IntToHex(Info.Value, 8), Start + 4 + 4 + 4, 4);
  Dec(Size, SizeOf(TSymbolConstInfo) + 2); // -2 ?!
  if Size > 0 then
  begin
    AddChild(Parent, 'Padding = ' + IntToStr(Size), FOffset, Size);
    Get(Size, mmPadding);
  end;
end;

procedure TTDSFile.ReadSourceModules(Size: Integer; Parent: TTDSNode);
var
  Module: TTDSNode;
  Start : Integer;
  Info  : ^TSourceModuleInfo;
  Count : Integer;
  Ofs   : ^TArrayOfCardinals;
  Index : Integer;
  Src   : ^TSourceFileEntry;
  Lines : ^TArrayOfCardinals;
  Rgn   : ^TArrayOfCardinalPairs;
  Seg   : ^TArrayOfWords;
  SNode : TTDSNode;
  LNode : TTDSNode;
  sIndex: Integer;
  lIndex: Integer;
  Line  : ^TSourceLineInfo;
  nIndex: Integer;
  Addr  : ^TArrayOfCardinals;
  LNum  : ^TArrayOfWords;
  Node  : TTDSNode;
begin
  Module := AddChild(Parent, 'Source', FOffset, Size);

  Start := FOffset;  // Adresse de base pour les informations ci-dessous

  // Info sur le module
  Info := Get(SizeOf(TSourceModuleInfo), mmSymbolInfo);
  // @ des Infos
  Ofs := Get(Info.FileCount * SizeOf(Cardinal), mmSymbolInfo);
  // Rangs dans le segment
  Rgn := Get(Info.SegmentCount * 2 * SizeOf(Cardinal), mmSymbolinfo);
  // Le segment
  Seg := Get(Info.SegmentCount * SizeOf(Word), mmSymbolInfo);

  Node := AddChild(
    Module,
    'Segments',
    Start + SizeOf(TSourceModuleInfo) + Info.FileCount * SizeOf(Cardinal),
    Info.SegmentCount * 2 * SizeOf(Cardinal)
  );
  AddDump(FModDump2, 'Segments',
    Start + SizeOf(TSourceModuleInfo) + Info.FileCount * SizeOf(Cardinal),
    Info.SegmentCount * 2 * SizeOf(Cardinal)
  );
  for sIndex := 0 to Info.SegmentCount - 1 do
  begin
    AddChild(
      Node,
      '#' + IntToStr(Seg[sIndex]) + ': ' + IntToHex(Rgn[sIndex].Start, 8) + '-' + IntToHex(Rgn[sIndex].Stop, 8),
      Start + SizeOf(TSourceModuleInfo) + Info.FileCount * SizeOf(Cardinal) + (sIndex - 1) * 2 * SizeOf(Cardinal),
      2 * SizeOf(Cardinal)
    );
  end;

  Count := Info.FileCount;
  for Index := 0 to Count - 1 do
  begin

    // Adresse du source
    FOffset := Start + Ofs[Index];

    // Le source
    Src := Get(SizeOf(TSourceFileEntry), mmSymbolInfo);
    // Les lignes
    Lines := Get(Src.LineCount * SizeOf(Cardinal), mmSymbolInfo);
    // Les régions concernées
    Rgn   := Get(Src.LineCount * 2 * SizeOf(Cardinal), mmSymbolInfo);

    Node := AddChild(Module, GetName(Src.NameIndex), FOffset - SizeOf(TSourceFileEntry), SizeOf(TSourceFileEntry));

    SNode := AddChild(Node, 'Lines', Start + Ofs[Index], SizeOf(TSourceFileEntry));

    for lIndex := 0 to Src.LineCount - 1 do
    begin
      FOffset := Start + Lines[lIndex];
      Line := Get(SizeOf(TSourceLineInfo), mmSymbolInfo);
      Addr := Get(Line.LineCount * SizeOf(Cardinal), mmSymbolInfo);
      LNum := Get(Line.LineCount * SizeOf(Cardinal), mmSymbolInfo);
      LNode := AddChild(sNode, 'Seg#' + IntToStr(Line.Segment), Start + Lines[lIndex], SizeOf(TSourceLineInfo));
      for nIndex := 0 to Line.LineCount - 1 do
      begin
        AddChild(LNode, 'Line ' + IntToStr(LNum[nIndex]) + ' : ' + IntToHex(Addr[nIndex], 8), FOffset, 0);
      end;
    end;
  end
end;

procedure TTDSFile.ReadUses(Size: Integer; Parent: TTDSNode);
var
  Start: Integer;
  Count: Integer;
  Info : ^TArrayOfCardinals;
  Index: Integer;
begin
  Start := FOffset;
  Count := Size div 4;
  Info := Get(Count * SizeOf(Cardinal), mmSymbolInfo);
  Parent := AddChild(Parent, 'Uses', Start - SizeOf(TSymbolInfo), Count * 4 + SizeOf(TSymbolInfo));
  for Index := 0 to Count - 1 do
  begin
    AddChild(Parent, GetName(Info[Index]), Start + Index * 4, 4);
  end;
end;

procedure TTDSFile.ReadUsing(Size: Integer; Parent: TTDSNode);
var
  Start: Integer;
  Count: Integer;
  Info : ^TArrayOfCardinals;
  Index: Integer;
begin
  Start := FOffset;
  Count := PWord(Get(2, mmSymbolInfo))^;
  Info := Get(Count * SizeOf(Cardinal), mmSymbolInfo);
  Parent := AddChild(Parent, 'Using', Start - SizeOf(TSymbolInfo), 2 + Count * 4 + SizeOf(TSymbolInfo));
  for Index := 0 to Count - 1 do
  begin
    AddChild(Parent, GetName(Info[Index]), Start + 2 + Index * 4, 4);
  end;
end;

procedure TTDSFile.ReadGlobalTypes(Size: Integer; Parent: TTDSNode);
begin
  AddChild(Parent, 'GlobalTypes', FOffset, Size);
end;

procedure TTDSFile.ReadLink(Size: Integer; Parent: TTDSNode);
var
  Start: Integer;
begin
  if Parent.Proc <> nil then
    Parent := Parent.Proc;
  Start := FOffset;
  Parent := AddChild(Parent, 'Link = ' + IntToStr(GetU32(mmSymbolInfo)), Start - SizeOf(TSymbolInfo), SizeOf(Integer) + SizeOf(TSymbolInfo));
end;

procedure TTDSFile.ReadNameSpaces(Size: Integer; Parent: TTDSNode);
var
  Start: Integer;
  Count: Integer;
  Info : ^TArrayOfCardinals;
  Index: Integer;
begin
  Start := FOffset;
  Count := Size div 4;
  Info := Get(Count * SizeOf(Cardinal), mmSymbolInfo);
  Parent := AddChild(Parent, 'NameSpace', Start - SizeOf(TSymbolInfo), Count * 4 + SizeOf(TSymbolInfo));
  for Index := 0 to Count - 2 do
  begin
    AddChild(Parent, GetName(Info[Index]), Start + Index * 4, 4);
  end;
end;

procedure TTDSFile.ReadOptVar(Size: Integer; Parent: TTDSNode);
var
  Start: Integer;
  Info : ^TOptVarInfo;
  Regs : ^TOptVarRegInfo;
  Index: Integer;
begin
  if Parent.Proc <> nil then
    Parent := Parent.Proc;
  Start := FOffset;
  Info := Get(SizeOf(TOptVarInfo), mmSymbolInfo);
  Parent := AddChild(Parent, 'registers ' + IntToStr(Info.Num), Start - SizeOf(TSymbolInfo), SizeOf(TOptVarInfo) + Info.Num * SizeOf(TOptVarRegInfo) + SizeOf(TSymbolInfo));
  for Index := 1 to Info.Num do
  begin
    Regs := Get(SizeOf(TOptVarRegInfo), mmSymbolInfo);
    AddChild(Parent, 'Reg #' + IntToStr(Regs.Register) + ' ' + IntToHex(Regs.Start, 4) + ' + ' + IntToStr(Regs.Size), Start, SizeOf(TOptVarRegInfo));
    Start := FOffset;
  end;
end;

procedure TTDSFile.ReadRegister(Size: Integer; Parent: TTDSNode);
var
  Start: Integer;
  Info : ^TRegisterInfo;
begin
  if Parent.Proc <> nil then
    Parent := Parent.Proc;
  Start := FOffset;
  Info := Get(SizeOf(TRegisterInfo), mmSymbolInfo);
  Parent := AddChild(Parent, 'register ' + GetName(Info.Name), Start - SizeOf(TSymbolInfo), SizeOf(TRegisterInfo) + SizeOf(TSymbolInfo));
  AddChild(Parent, 'Kind = ' + GetKind(Info.Kind), Start, 4);
  AddChild(Parent, 'Register = ' + IntToStr(Info.Register), Start + 4, 2);
  AddChild(Parent, 'BrowserOfs = ' + IntToStr(Info.BrowserOfs), Start + 4 + 2 + 4, 4);
end;

{ TTDSNode }

procedure TTDSNode.SetEnd(AOffset: Integer);
begin
  Size := AOffset - Offset;
  Text := Copy(Text, 1, 9) + IntToHex(AOffset, 8) + Copy(Text, 18, MaxInt);
end;

end.
