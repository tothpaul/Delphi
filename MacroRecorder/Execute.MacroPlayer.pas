unit Execute.MacroPlayer;
{
  MacroRecord (c)2019 Execute SARL
}
interface

{$DEFINE SINGLETON}
{$DEFINE CONST_AS_COMMENT}

{$IFDEF DEBUG}
{.$DEFINE LOG}
{.$DEFINE KEYS}
{.$DEFINE MERGE}
{.$DEFINE SIMULATE}
{$ENDIF}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  System.SysUtils,
  System.Types,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.ExtCtrls;

type
  TEvent = record
    IType: Integer;
    Pt   : TPoint;
    Code : Cardinal;
    Flags: Word;
    Time : Cardinal;
  end;
  PEvent = ^TEvent;

  PEventPool = ^TEventPool;
  TEventPool = record
    Count: Integer;
    Items: array[0..1023] of TEvent;
    Next : PEventPool;
  end;

  TMacroPlayerStatus = (
    mpsIdle,
    mpsRecord,
    mpsPlayback,
    mpsShedule
  );

  TMacroPlayerOptions = (
    mpoRecordMouseClick,
    mpoRecordMouseMove,
    mpoRecordKeyboard,
    mpoUnicodeChar,
    mpoMergeChars,
    mpoSleep,
    mpoProcessMessages
  );
  TMacroPlayerOption = set of TMacroPlayerOptions;

  TEatKey = record
    Code: Word;
    Eat : Boolean;
  end;

  TMacroPlayer = class(TComponent)
  private
    FOptions: TMacroPlayerOption;
    FMoveThreshold: Integer;
    FThread: TThread;
    FEvents: PEventPool;
    FRecord: PEventPool;
    FPlaying: PEventPool;
    FPlaypos: Integer;
    FTiming : DWORD;
    FStatus: TMacroPlayerStatus;
    FJournal: HHOOK;
    FText   : string;
    FStrings: TStringList;
    FKeyDown: TMsg;
    // handle: WM_KEYDOWN(a), WM_CHAR(a), WM_KEYDOWN(b), WH_CMAR(b), WM_KEYUP(a), WM_KEYUP(b)
    FEatKeys : array[0..5] of TEatKey; // 6 keys at the same time ... should be sufficient
    FEatCount: Integer;
    FIgnoreHWnd: THandle;
    FOnPlayDone: TNotifyEvent;
    function HasEvent: Boolean;
    function DoEvent: Cardinal;
    function NewEvent: PEvent;
    function LastEvent: PEvent;
    procedure MergeUnicode;
    procedure AddEvent(var Msg: TMsg);
    procedure PlayEvent(Event: TEvent);
    procedure MessageEvent(Msg: TMsg);
    procedure SetStatus(const Value: TMacroPlayerStatus);
    procedure EatKey(Code: Word);
    function EatedKey(Code: Word): Boolean;
    procedure DoSleep(Delay: Cardinal);
    procedure ClickScreen(const Point: TPoint; Button: Cardinal);
    function EventCount: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartRecord;
    procedure StopRecord;
    procedure PlayBack;
    function Playing: Boolean;
    procedure Shedule;
    procedure ProcessEvents;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToScript(const AFileName: string);
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStrings(Strings: TStrings);
    procedure SaveAsCode(Strings: TStrings);
    procedure MoveMouse(x, y: Integer; Time: Cardinal);
    procedure ClickMouse(x, y: Integer; Button, Time: Cardinal);
    procedure PressKey(vkCode, Flags, Time: Cardinal);
    procedure TypeString(const Str: string; Time: Cardinal);
    procedure ControlClick(Control: TControl);
    procedure Toggle(Option: TMacroPlayerOptions);
    property Status: TMacroPlayerStatus read FStatus write SetStatus;
    property Options: TMacroPlayerOption read FOptions write FOptions;
    property MoveThreshold: Integer read FMoveThreshold write FMoveThreshold;
    property IgnoreHWnd: THandle read FIgnoreHwnd write FIgnoreHWnd;
    property OnPlayDone: TNotifyEvent read FOnPlayDone write FOnPlayDone;
  end;

implementation

const
  KEYEVENTF_KEYDOWN = 0;
  KEYEVENTF_STRING = 16;

function HasFlag(var Flags: DWORD; Bits: DWORD): Boolean;
begin
  Result := (Bits > 0) and ((Flags and Bits) = Bits);
  if Result then
    Dec(Flags, Bits);
end;

procedure AddFlag(var Str: string; Name: string);
begin
  if Str <> '' then
    Str := Str + ' OR ';
  Str := Str + Name;
end;

function MouseFlags(Flags: DWORD): string;
{$IFDEF CONST_AS_COMMENT}
var
  iFlags: DWORD;
{$ENDIF}
begin
{$IFDEF CONST_AS_COMMENT}
  iFlags := Flags;
{$ENDIF}
  Result := '';
  if HasFlag(Flags, MOUSEEVENTF_ABSOLUTE) then AddFlag(Result, 'MOUSEEVENTF_ABSOLUTE');
  if HasFlag(Flags, MOUSEEVENTF_MOVE) then AddFlag(Result, 'MOUSEEVENTF_MOVE');
  if HasFlag(Flags, MOUSEEVENTF_LEFTDOWN) then AddFlag(Result, 'MOUSEEVENTF_LEFTDOWN');
  if HasFlag(Flags, MOUSEEVENTF_LEFTUP) then AddFlag(Result, 'MOUSEEVENTF_LEFTUP');
  if HasFlag(Flags, MOUSEEVENTF_RIGHTDOWN) then AddFlag(Result, 'MOUSEEVENTF_RIGHTDOWN');
  if HasFlag(Flags, MOUSEEVENTF_RIGHTUP) then AddFlag(Result, 'MOUSEEVENTF_RIGHTUP');
{$IFDEF CONST_AS_COMMENT}
  if Result = '' then
    Result := IntToStr(iFlags)
  else
    Result := IntToStr(iFlags) + ' { ' + Result + ' } ';
{$ELSE}
  if Flags <> 0 then AddFlag(Result, IntToStr(Flags));
  if Result = '' then
    Result := '0';
{$ENDIF}
end;

function KeyFlags(Flags: DWORD): string;
{$IFDEF CONST_AS_COMMENT}
var
  iFlags: DWORD;
{$ENDIF}
begin
{$IFDEF CONST_AS_COMMENT}
  iFlags := Flags;
{$ENDIF}
  Result := '';
  if HasFlag(Flags, KEYEVENTF_UNICODE) then AddFlag(Result, 'KEYEVENTF_UNICODE');
  if HasFlag(Flags, KEYEVENTF_SCANCODE) then AddFlag(Result, 'KEYEVENTF_SCANCODE');
  if HasFlag(Flags, KEYEVENTF_STRING) then AddFlag(Result, 'KEYEVENTF_STRING');
  if HasFlag(Flags, KEYEVENTF_KEYUP) then AddFlag(Result, 'KEYEVENTF_KEYUP');// else AddFlag(Result, 'KEYEVENTF_KEYDOWN');
  if HasFlag(Flags, KEYEVENTF_EXTENDEDKEY) then AddFlag(Result, 'KEYEVENTF_EXTENDEDKEY');
{$IFDEF CONST_AS_COMMENT}
  if Result = '' then
    Result := IntToStr(iFlags)
  else
    Result := IntToStr(iFlags) + ' { ' + Result + ' } ';
{$ELSE}
  if Flags <> 0 then AddFlag(Result, IntToStr(Flags));
  if Result = '' then
    Result := '0';
{$ENDIF}
end;

function VirtualKeyName(Code: WORD): string;
begin
  case Code of
    VK_UP      : Result := 'VK_UP';
    VK_DOWN    : Result := 'VK_DOWN';
    VK_LEFT    : Result := 'VK_LEFT';
    VK_RIGHT   : Result := 'VK_RIGHT';
    VK_HOME    : Result := 'VK_HOME';
    VK_END     : Result := 'VK_END';
    VK_PRIOR   : Result := 'VK_PRIOR';
    VK_NEXT    : Result := 'VK_NEXT';
    VK_SHIFT   : Result := 'VK_SHIFT';
    VK_CONTROL : Result := 'VK_CONTROL';
  else
    Result := '';
  end;
end;

function VirtualKey(Code: WORD): string;
begin
  Result := VirtualKeyName(Code);
  if Result = '' then
  begin
    Result := IntToStr(Code);
    case Code of
      Ord('0')..Ord('9'),
      Ord('A')..Ord('Z'): Result := Result + ' { ' + Char(Code) + ' } ';
    end;
  end
{$IFDEF CONST_AS_COMMENT}
  else
    Result := IntToStr(Code) + ' { ' + Result + ' } ';
{$ENDIF}
end;

function CharName(Code: Word): string;
begin
  case Code of
    0..31:
      Result := '#' + IntToStr(Code);
  else
    Result := '''' + Char(Code) + '''';
  end;
end;

function PascalString(const Str: string): string;
var
  quote: Boolean;
  Index: Integer;
begin
  if Str = '' then
    Exit('''''');
  quote := False;
  Result := '';
  for Index := 1 to Length(Str) do
  begin
    if Str[Index] < ' ' then
    begin
      if quote then
      begin
        Result := Result + '''';
        Quote := False;
      end;
      Result := Result + '#' + IntToStr(Ord(Str[Index]));
    end else begin
      if not quote then
      begin
        Result := Result + '''';
        Quote := True;
      end;
      Result := Result + Str[Index];
      if Str[Index] = '''' then
        Result := Result + '''';
    end;
  end;
  if quote then
  begin
    Result := Result + '''';
  end;
end;

{$IFDEF SIMULATE}
procedure SendInput(Count: Integer; const Input: TInput; Size: Integer);
begin
  Assert(Count = 1);
  AllocConsole;
  case Input.Itype of
    INPUT_MOUSE:
    begin
      WriteLn('SendInput(INPUT_MOUSE, ', Input.mi.dx, ', ', Input.mi.dy,', ', MouseFlags(Input.mi.dwFlags),')');
    end;
    INPUT_KEYBOARD:
    begin
      WriteLn('SendInput(INPUT_KEYBOARD, ', VirtualKey(Input.ki.wVk), ', ', Input.ki.wScan, ', ', KeyFlags(Input.ki.dwFlags), ')');
      if Input.ki.dwFlags = KEYEVENTF_UNICODE then
        WriteLn(' --> ', Char(Input.ki.wScan));
    end;
  else
    WriteLn(Input.IType, ' ???)');
  end;
end;
{$ENDIF}
var
{$IFDEF SINGLETON}
  Instance: TMacroPlayer;
{$ELSE}
  Players: TList = nil;
{$ENDIF}

type
  TPlayerThread = class(TThread)
    Player: TMacroPlayer;
    Delay : Cardinal;
    constructor Create(APlayer: TMacroPlayer);
    procedure Execute; override;
    procedure DoEvent;
    procedure Done;
  end;

constructor TPlayerThread.Create(APlayer: TMacroPlayer);
begin
  Player := APlayer;
  inherited Create(False);
  FreeOnTerminate := True;
end;

procedure TPlayerThread.Execute;
begin
  {$IFDEF LOG}WriteLn('Thread start');{$ENDIF}
  try
    while Player.HasEvent do
    begin
      Synchronize(DoEvent);
      Sleep(Delay);
    end;
    {$IFDEF LOG}WriteLn('done');{$ENDIF}
  finally
    Synchronize(Done);
  end;
end;

procedure TPlayerThread.DoEvent;
begin
  Delay := Player.DoEvent;
end;

procedure TPlayerThread.Done;
begin
  Player.FThread := nil;
  if Assigned(Player.FOnPlayDone) then
    Player.FOnPlayDone(Player);
  {$IFDEF LOG}WriteLn('Thread done');{$ENDIF}
end;

procedure ClearEvents(var Events: PEventPool);
begin
  while Events <> nil do
  begin
    Dispose(Events);
    Events := Events.Next;
  end;
end;

function GetMessageHook(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  M: PMsg;
begin
//  WriteLn('Hook ', nCode, ', ', wParam, ', ', lParam);
  if (nCode = HC_ACTION) and (wParam = PM_REMOVE) then
  begin
    M := PMsg(lParam);
  {$IFDEF SINGLETON}
    Assert(Instance <> nil);
    Instance.MessageEvent(M^);
  {$ELSE}
    for I := 0 to Players.Count - 1 do
      TMacroPlayer(Players[I]).MessageEvent(M^);
  {$ENDIF}
  end else begin
    Assert((nCode = HC_ACTION) or (wParam = PM_NOREMOVE));
  end;
  Result := CallNextHookEx(0, nCode, wParam, lParam);
end;

{ TMacroPlayer }
constructor TMacroPlayer.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF LOG}AllocConsole;{$ENDIF}
  FStrings := TStringList.Create;
  FOptions := [mpoRecordMouseClick, mpoRecordMouseMove, mpoRecordKeyboard, mpoUnicodeChar, mpoMergeChars, mpoSleep, mpoProcessMessages];
  FMoveThreshold := 5;
end;

destructor TMacroPlayer.Destroy;
begin
  Status := mpsIdle;
{$IFDEF SINGLETON}
  Instance := nil;
{$ENDIF}
  FStrings.Free;
  inherited;
end;

function TMacroPlayer.DoEvent: Cardinal;
begin
  PlayEvent(FPlaying.Items[FPlayPos]);
  Inc(FPlayPos);
  if HasEvent then
  begin
    Result := FPlaying.Items[FPlayPos].Time;
  end else begin
    Result := 0;
  end;
end;

procedure TMacroPlayer.DoSleep(Delay: Cardinal);
begin
  if (mpoProcessMessages in FOptions) and (FStatus <> mpsPlayback) then
    Application.ProcessMessages; // NB: le Playback se fait dans un Thread secondaire, pas de ProcessMessages !
  if (mpoSleep in FOptions) and (FStatus <> mpsPlayback) then
    Sleep(Delay); // le Sleep() se fait dans le Thread !
end;

function TMacroPlayer.EatedKey(Code: Word): Boolean;
var
  Index: Integer;
begin
  for Index := 0 to FEatCount - 1 do
  begin
    if FEatKeys[Index].Code = Code then
    begin
      Result := FEatKeys[Index].Eat;
      FEatKeys[Index].Eat := False;
      Exit;
    end;
  end;
  while (FEatCount > 0) and (FeatKeys[FEatCount - 1].Eat = False) do
    Dec(FEatCount);
  Result := False;
end;

procedure TMacroPlayer.EatKey(Code: Word);
var
  Index: Integer;
begin
  for Index := 0 to FEatCount - 1 do
  begin
    if FEatKeys[Index].Code = Code then
    begin
      FEatKeys[Index].Eat := True;
      Exit;
    end;
  end;
  if FEatCount < Length(FEatKeys) then
  begin
    FEatKeys[FEatCount].Code := Code;
    FEatKeys[FEatCount].Eat := True;
    Inc(FEatCount);
{$IFDEF LOG}
  end else begin
    WriteLn('EATKEYS IF FULL !!');
{$ENDIF}
  end;
end;

function TMacroPlayer.EventCount: Integer;
var
  E: PEventPool;
begin
  Result := 0;
  E := FEvents;
  while E <> nil do
  begin
    Inc(Result, E.Count);
    E := E.Next;
  end;
end;

function TMacroPlayer.HasEvent: Boolean;
begin
  if FStatus <> mpsPlayback then
    Exit(False);
  while (FPlaying <> nil) and (FPlayPos >= FPlaying.Count) do
  begin
    Dec(FPlayPos, FPlaying.Count);
    FPlaying := FPlaying.Next;
  end;
  Result := FPlaying <> nil;
  if Result = False then
    Status := mpsIdle;
end;

function TMacroPlayer.NewEvent: PEvent;
begin
  if (FEvents = nil) or (FRecord.Count > High(FRecord.Items)) then
  begin
    if FEvents = nil then
    begin
      New(FEvents);
      FRecord := FEvents;
    end else begin
      New(FRecord.Next);
      FRecord := FRecord.Next;
    end;
    FRecord.Count := 0;
    FRecord.Next := nil;
  end;
  Result := @FRecord.Items[FRecord.Count];
  Inc(FRecord.Count);
end;

function TMacroPlayer.LastEvent: PEvent;
begin
  if (FRecord = nil) or (FRecord.Count = 0) then
    Result := nil
  else begin
    Result := @FRecord.Items[FRecord.Count - 1];
  end;
end;

procedure TMacroPlayer.LoadFromFile(const AFileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMacroPlayer.LoadFromStream(AStream: TStream);
var
  Count: Integer;
  S: string;
  E: PEventPool;
  N: PEventPool;
begin
  Status := mpsIdle;
  ClearEvents(FEvents);
  FStrings.Clear;

  AStream.Read(Count, SizeOf(Count));
  // Version tag
  if Count = Integer($CAFE0001) then
  begin
  // strings
    AStream.Read(Count, SizeOf(Count));
    SetLength(S, Count);
    AStream.Read(S[1], Count * SizeOf(Char));
    FStrings.Text := S;
  // Event count
    AStream.Read(Count, SizeOf(Count));
  end else begin
    FStrings.Clear;
  end;

  while Count > 0 do
  begin
    New(E);
    E.Next := nil;
    if FEvents = nil then
      FEvents := E
    else
      N.Next := E;
    N := E;
    if Count > Length(E.Items) then
      E.Count := Length(E.Items)
    else
      E.Count := Count;
    AStream.Read(E.Items, E.Count * SizeOf(TEvent));
    Dec(Count, E.Count);
  end;
end;

procedure TMacroPlayer.MessageEvent(Msg: TMsg);
begin
  Assert(Self is TMacroPlayer);
  if FStatus <> mpsRecord then
  begin
    {$IFDEF LOG}WriteLn('MessageEvent ?!');{$ENDIF}
    Exit;
  end;

  if Msg.hwnd = FIgnoreHWnd then
    Exit;

  case Msg.message of

    WM_KEYDOWN,
    WM_CHAR,
    WM_KEYUP,
    WM_SYSKEYDOWN,
    WM_SYSCHAR,
    WM_SYSDEADCHAR,
    WM_SYSKEYUP:
    begin
      if mpoRecordKeyboard in FOptions then
        AddEvent(Msg)
     {$IFDEF KEYS}
       else
         WriteLn(' NO KEYS !');
     {$ENDIF};
    end;

    WM_MOUSEMOVE,
    WM_NCMOUSEMOVE:
    begin
      if mpoRecordMouseMove in FOptions then
        AddEvent(Msg);
    end;

    WM_LBUTTONDOWN,
    WM_LBUTTONDBLCLK,
    WM_LBUTTONUP,

    WM_RBUTTONDOWN,
    WM_RBUTTONDBLCLK,
    WM_RBUTTONUP,

    WM_MBUTTONDOWN,
    WM_MBUTTONDBLCLK,
    WM_MBUTTONUP,

    WM_NCLBUTTONDOWN,
    WM_NCLBUTTONUP:
    begin
      FKeyDown.lParam := 0;
      if mpoRecordMouseClick in FOptions then
        AddEvent(Msg);
    end;

//    WM_MOUSEWHEEL,

    WM_PAINT,
    WM_TIMER: { ignorer };
  else
//    WriteLn(MessageName(Msg.message));
  end;
end;

procedure TMacroPlayer.AddEvent(var Msg: TMsg);
var
  E: PEvent;
begin
  {$IFDEF LOG}
    if Msg.message <> WM_MOUSEMOVE then
    begin
      Write('AddEvent ', {MessageName}(Msg.message), ', wParam = ', Msg.wParam,', lParam = ', IntToHex(Cardinal(Msg.lParam), 8));
      case Msg.message of
        WM_KEYDOWN,
        WM_KEYUP,
        WM_SYSKEYDOWN,
        WM_SYSKEYUP  : Write(', ScanCode = ', VirtualKey(Msg.wParam), ', OEM = ', (Msg.lParam shr 16) and $FF);
        WM_CHAR,
        WM_SYSCHAR,
        WM_SYSDEADCHAR :
        begin
          Write(', ScanCode = ', VirtualKey(Msg.wParam), ', OEM = ', (Msg.lParam shr 16) and $FF, ', Char = ');
          if Msg.wParam < 32 then Write('#', Msg.wParam) else Write(Char(Msg.wParam));
        end;
      end;
      WriteLn;
    end;
  {$ELSE}
  {$IFDEF KEYS}
      case Msg.message of
        WM_KEYDOWN,
        WM_KEYUP,
        WM_SYSKEYDOWN,
        WM_SYSKEYUP  :
        begin
          WriteLn({MessageName}(Msg.Message), ', ScanCode = ', VirtualKey(Msg.wParam), ', OEM = ', (Msg.lParam shr 16) and $FF);
        end;
        WM_CHAR,
        WM_SYSCHAR,
        WM_SYSDEADCHAR :
        begin
          Write({MessageName}(Msg.Message), ' OEM = ', (Msg.lParam shr 16) and $FF, ', Char = ');
          if Msg.wParam < 32 then Write('#', Msg.wParam) else Write(Char(Msg.wParam));
          WriteLn;
        end;
      end;
  {$ENDIF}
  {$ENDIF}
  Winapi.Windows.ScreenToClient(GetActiveWindow, Msg.pt);

  // Optimise MouseMove
  if (FMoveThreshold > 0) and (FEvents <> nil) and ((Msg.message = WM_MOUSEMOVE) or (Msg.message = WM_NCMOUSEMOVE)) then
  begin
    E := @FRecord.Items[FRecord.Count - 1];
    if (E.IType = INPUT_MOUSE) and (E.Flags = MOUSEEVENTF_MOVE)
    and (Abs(E.Pt.X - Msg.pt.X) < FMoveThreshold)
    and (Abs(E.Pt.Y - Msg.pt.Y) < FMoveThreshold) then
      Exit;
  end;

  // WM_CHAR après WM_KEYDOWN
  if (Msg.message = WM_CHAR) or (Msg.message = WM_SYSCHAR) or (Msg.message = WM_SYSDEADCHAR) then
  begin
    if not (mpoUnicodeChar in FOptions) then
      Exit;

    {$IFDEF MERGE}WriteLn('WM_CHAR ', Char(Msg.wParam),' / Text = ', FText);{$ENDIF}

    case Msg.wParam of
       0.. 8,
      10..31:
      begin
      {$IFDEF MERGE}WriteLn('WM_CHAR < 32, FKeyDown.lParam = 0');{$ENDIF}
        FText := ''; // rupture dans la chaîne
        Exit;
      end;
    end;

    if (FKeyDown.lParam and $1FFFFFF) = (Msg.lParam and $1FFFFFF) then
    begin
      //{$IFDEF UNICODE}WriteLn('UNICODE');{$ENDIF}
      E := LastEvent;
      if E = nil then
      begin
      {$IFDEF MERGE}WriteLn('LastEvent is nil');{$ENDIF}
        Exit;
      end;
      if (E.Flags = KEYEVENTF_KEYDOWN) and (E.Code = FKeyDown.wParam) then
      begin
       {$IFDEF MERGE}WriteLn('Last.KEYDOWN -> UNICODE');{$ENDIF}
       {$IFDEF KEYS}WriteLn(' UNICODE ', Char(Msg.wParam));{$ENDIF}
        EatKey(FKeyDown.wParam);
        E.Flags := KEYEVENTF_UNICODE;
        E.Code := Msg.wParam;
        MergeUnicode;
      end
//    end else begin
//      WriteLn('(FKeyDown.lParam and $1FFFFFF) <> (Msg.lParam and $1FFFFFF) => ', (FKeyDown.lParam and $1FFFFFF),' <> ',(Msg.lParam and $1FFFFFF));
    end;
    Exit;
  end;

  // WM_KEYUP après un WM_KEYDOWN sans WM_CHAR
  if (Msg.message = WM_KEYUP) or (Msg.message = WM_SYSKEYUP) then
  begin
    if (mpoUnicodeChar in FOptions) and EatedKey(Msg.wParam) then
    begin
      {$IFDEF KEYS}WriteLn(' EATED');{$ENDIF}
      Exit;
    end;
  end;

  if FEvents = nil then
    FTiming := Msg.time;

  E := NewEvent;
  E.Pt := Msg.pt;
  E.Code := Msg.wParam;

  case Msg.message of

    WM_KEYDOWN,
    WM_SYSKEYDOWN:
    begin
      FKeyDown := Msg;
      E.IType := INPUT_KEYBOARD;
      case E.Code of
        VK_UP,
        VK_DOWN,
        VK_LEFT,
        VK_RIGHT,
        VK_HOME,
        VK_END,
        VK_PRIOR,
        VK_NEXT,
        VK_INSERT,
        VK_DELETE:
          E.Flags := KEYEVENTF_EXTENDEDKEY;
      else
          E.Flags := 0;
      end;
    end;
    WM_KEYUP,
    WM_SYSKEYUP:
    begin
      E.IType := INPUT_KEYBOARD;
      case E.Code of
        VK_UP,
        VK_DOWN,
        VK_LEFT,
        VK_RIGHT,
        VK_HOME,
        VK_END,
        VK_PRIOR,
        VK_NEXT,
        VK_INSERT,
        VK_DELETE:
          E.Flags := KEYEVENTF_KEYUP or KEYEVENTF_EXTENDEDKEY;
      else
          E.Flags := KEYEVENTF_KEYUP;
      end;
    end;

    WM_LBUTTONDOWN,
    WM_LBUTTONDBLCLK,
    WM_NCLBUTTONDOWN:
    begin
      E.IType := INPUT_MOUSE;
      E.Flags := MOUSEEVENTF_LEFTDOWN;
    end;
    WM_LBUTTONUP,
    WM_NCLBUTTONUP:
    begin
      E.IType := INPUT_MOUSE;
      E.Flags := MOUSEEVENTF_LEFTUP;
    end;

    WM_RBUTTONDOWN,
    WM_RBUTTONDBLCLK:
    begin
      E.IType := INPUT_MOUSE;
      E.Flags := MOUSEEVENTF_RIGHTDOWN;
    end;
    WM_RBUTTONUP:
    begin
      E.IType := INPUT_MOUSE;
      E.Flags := MOUSEEVENTF_RIGHTUP;
    end;

    WM_MBUTTONDOWN,
    WM_MBUTTONDBLCLK:
    begin
      E.IType := INPUT_MOUSE;
      E.Flags := MOUSEEVENTF_MIDDLEDOWN;
    end;
    WM_MBUTTONUP:
    begin
      E.IType := INPUT_MOUSE;
      E.Flags := MOUSEEVENTF_MIDDLEUP;
    end;

    WM_MOUSEMOVE,
    WM_NCMOUSEMOVE:
    begin
      E.IType := INPUT_MOUSE;
      E.Flags := MOUSEEVENTF_MOVE;
    end

  {$IFDEF LOG}
  else
    WriteLn({MessageName}(Msg.message), ' ???');
  {$ENDIF}
  end;
  if Msg.time > FTiming then
  begin
    E.Time := Msg.time - FTiming;
    FTiming := Msg.time;
  end else begin
    E.Time := 0;
  end;
end;

procedure TMacroPlayer.MergeUnicode;
var
  E1, E2: PEvent;
begin
  if (mpoMergeChars in FOptions) and (FRecord.Count > 1) then
  begin
    E1 := @FRecord.Items[FRecord.Count - 2];
    if E1.IType <> INPUT_KEYBOARD then
      Exit;
    E2 := @FRecord.Items[FRecord.Count - 1];
    Assert(E2.IType = INPUT_KEYBOARD);
    Assert(E2.Flags = KEYEVENTF_UNICODE);
    if E1.Flags = KEYEVENTF_UNICODE then
    begin
      FText := Char(E1.Code);
      E1.Flags := KEYEVENTF_STRING;
      E1.Code := FStrings.Add(FText);
      {$IFDEF MERGE}WriteLn('NewString at ', E1.Code, ' = ', FText);{$ENDIF}
    end;
    if E1.Flags = KEYEVENTF_STRING then
    begin
      FText := FText + Char(E2.Code);
      {$IFDEF MERGE}WriteLn('MargeString at ', E1.Code, ' = ', FText);{$ENDIF}
      FStrings[E1.Code] := FText;
      Dec(FRecord.Count);
    {$IFDEF KEYS}WriteLn('MERGED ', FRecord.Count);{$ENDIF}
    end;
  end;
end;

procedure TMacroPlayer.PlayEvent(Event: TEvent);
{-$DEFINE INPUT}
{$IFDEF INPUT}
var
  Input: TInput;
{$ENDIF}
begin
//  WriteLn('Play ', MessageName(Msg.message));
  case Event.IType of
    INPUT_MOUSE:
    begin
    {$IFDEF INPUT}
      Winapi.Windows.ClientToScreen(GetActiveWindow, Event.pt);
      SetCursorPos(Event.Pt.X, Event.Pt.Y);
      if Event.Flags = MOUSEEVENTF_MOVE then
      begin
        Exit;
      end else begin
        Input.Itype := INPUT_MOUSE;
        Input.mi.dx := (65535 * Event.pt.x) div Screen.Width;
        Input.mi.dx := (65535 * Event.pt.y) div Screen.Height;
        Input.mi.mouseData := 0;
        Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or Event.Flags;
        Input.mi.time := 0;
        Input.mi.dwExtraInfo := 0;
      end;
     {$ELSE}
      if Event.Flags = MOUSEEVENTF_MOVE then
        MoveMouse(Event.Pt.X, Event.Pt.Y, Event.Time)
      else
        ClickMouse(Event.Pt.X, Event.Pt.Y, Event.Flags, Event.Time);
     {$ENDIF}
    end;
    INPUT_KEYBOARD:
    begin
    {$IFDEF INPUT}
      Input.Itype := INPUT_KEYBOARD;
      Input.ki.dwFlags := Event.Flags;
      Input.ki.time := 0;
      Input.ki.dwExtraInfo := 0;
      if Event.Flags = KEYEVENTF_UNICODE then
      begin
        Input.ki.wVk := 0;
        Input.ki.wScan := Event.Code;
      end else
      if Event.Flags = KEYEVENTF_STRING then
      begin
        FText := FStrings[Event.Code];
        Input.ki.wVk := 0;
        Input.ki.wScan := Ord(FText[1]);
        Input.ki.dwFlags := KEYEVENTF_UNICODE;
      end else
      begin
        Input.ki.wVk := Event.Code;
        Input.ki.wScan := 0;
      end;
    {$ELSE}
      case Event.Flags of
        KEYEVENTF_UNICODE: TypeString(Char(Event.Code), Event.Time);
        KEYEVENTF_STRING : TypeString(FStrings[Event.Code], Event.Time);
      else
        PressKey(Event.Code, Event.Flags, Event.Time);
      end;
    {$ENDIF}
    end;
  else
    {$IFDEF LOG}WriteLn('Unknow EVENT ', Event.IType, ' !!!!');{$ENDIF}
    Exit;
  end;
  //SendInput(1, Input, SizeOf(Input));
{$IFDEF INPUT}
  SendInput(1, Input, SizeOf(Input));
{$ENDIF}
end;

procedure TMacroPlayer.SaveAsCode(Strings: TStrings);
var
  P: PEventPool;
  E: ^TEvent;
  I: Integer;
begin
  Strings.Add('program Macro;');
{$IFNDEF CONST_AS_COMMENT}
  Strings.Add('const');
  Strings.Add('  MOUSEEVENTF_LEFTDOWN  = ' + IntToStr(MOUSEEVENTF_LEFTDOWN) + ';');
  Strings.Add('  MOUSEEVENTF_LEFTUP    = ' + IntToStr(MOUSEEVENTF_LEFTUP) + ';');
  Strings.Add('  MOUSEEVENTF_RIGHTDOWN = ' + IntToStr(MOUSEEVENTF_RIGHTDOWN) + ';');
  Strings.Add('  MOUSEEVENTF_RIGHTUP   = ' + IntToStr(MOUSEEVENTF_RIGHTUP) + ';');
  Strings.Add('  KEYEVENTF_KEYUP       = ' + IntToStr(KEYEVENTF_KEYUP) + ';');
{$ENDIF}
  Strings.Add('var');
  Strings.Add('  MPlayer: TMacroPlayer;');
  Strings.Add('begin');
  Strings.Add('  MPlayer := TMacroPlayer.Create(nil);');
  Strings.Add('  MPlayer.Shedule;');
  P := FEvents;
  while P <> nil do
  begin
    for I := 0 to P.Count - 1 do
    begin
      E := @P.Items[I];
      case E.IType of
        INPUT_KEYBOARD:
        begin
          case E.Flags of
            KEYEVENTF_UNICODE : Strings.Add('  MPlayer.TypeString(' + CharName(E.Code)  + ', ' + IntToStr(E.Time) + ');');
            KEYEVENTF_STRING  : Strings.Add('  MPlayer.TypeString(' + PascalString(FStrings[E.Code]) + ', ' + IntToStr(E.Time) + ');');
          else
            Strings.Add('  MPlayer.PressKey(' + VirtualKey(E.Code) + ', ' + KeyFlags(E.Flags) + ', ' + IntToStr(E.Time) + ');');
          end;
        end;
        INPUT_MOUSE:
        begin
          case E.Flags of
            MOUSEEVENTF_MOVE: Strings.Add('  MPlayer.MoveMouse(' + IntToStr(E.Pt.X) + ', ' + IntToStr(E.Pt.Y) + ', ' + IntToStr(E.Time) + ');');
          else
            Strings.Add('  MPlayer.ClickMouse(' + IntToStr(E.Pt.X) + ', ' + IntToStr(E.Pt.Y) + ', ' + MouseFlags(E.Flags) + ', ' + IntToSTr(E.Time) + ');');
          end;
        end;
      end;
    end;
    P := P.Next;
  end;
  Strings.Add('');
  Strings.Add('  if Self <> nil then');
  Strings.Add('    Self.WindowState := wsMinimized;');
  Strings.Add('  FMain.SetFocus;');
  Strings.Add('  MPlayer.ProcessEvents();');
  Strings.Add('  MPlayer.Free;');
  Strings.Add('end.');
end;

procedure TMacroPlayer.SaveToFile(const AFileName: string);
var
  Stream: TFileStream;
begin
  if FEvents = nil then
    Exit;
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMacroPlayer.SaveToScript(const AFileName: string);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SaveAsCode(SL);
    SL.SaveToFile(AFileName);
  finally
    SL.Free;
  end;
end;

procedure TMacroPlayer.SaveToStream(AStream: TStream);
var
  Str  : string;
  Count: Integer;
  E: PEventPool;
begin
  if FStrings.Count > 0 then
  begin
    // Marquer de version
    Count := $CAFE0001;
    AStream.Write(Count, SizeOf(Count));
    // Les chaînes
    Str := FStrings.Text;
    Count := Length(Str);
    AStream.Write(Count, SizeOf(Count));
    AStream.Write(Str[1], Count * SizeOf(Char));
  end;
  // Les events
  Count := EventCount;
  AStream.Write(Count, SizeOf(Count));
  E := FEvents;
  while E <> nil do
  begin
    AStream.Write(E.Items, E.Count * SizeOf(TEvent));
    E := E.Next;
  end;
end;

procedure TMacroPlayer.SaveToStrings(Strings: TStrings);
var
  P: PEventPool;
  E: ^TEvent;
  I: Integer;
  C: string;
  F: string;
  S: string;
begin
  Strings.Add('Macro:');
  P := FEvents;
  while P <> nil do
  begin
    for I := 0 to P.Count - 1 do
    begin
      E := @P.Items[I];
      C := IntToStr(E.Code);
      S := '';
      F := '';
      case E.IType of
        INPUT_KEYBOARD :
          begin
            F := KeyFlags(E.Flags);
            case E.Flags of
              KEYEVENTF_UNICODE : S := ' -> ' + Char(E.Code);
              KEYEVENTF_STRING  : S := ' -> ' + FStrings[E.Code];
            else
              C := VirtualKey(E.Code);
            end;
          end;
        INPUT_MOUSE :
        begin
          F := MouseFlags(E.Flags);
        end;
      end;
      Strings.Add(
        'X = ' + IntToStr(E.Pt.X)
      + ', Y = ' + IntToStr(E.Pt.Y)
      + ', IType = ' + IntToStr(E.IType)
      + ', Code = ' + C
      + ', Flags = ' + F
      + ', time = ' + IntToStr(E.Time)
      + S
      );
    end;
    P := P.Next;
  end;
end;

procedure TMacroPlayer.SetStatus(const Value: TMacroPlayerStatus);
begin
// no change
  if Value = FStatus then
    Exit;

// security
  if (FStatus = mpsPlayback) and (Value <> mpsIdle) then
    Exit;


// apply status
  FStatus := Value;

// reset everything because the status has changed
  FKeyDown.lParam := 0;
  FText := '';

  if FJournal <> 0 then
  begin
    UnhookWindowsHookEx(FJournal);
    FJournal := 0;
  end;

{$IFDEF SINGLETON}
  if Instance = Self then
    Instance := nil;
{$ELSE}
  if Players <> nil then
  begin
    Players.Remove(Self);
    if Players.Count = 0 then
      FreeAndNil(Players);
  end;
{$ENDIF}

  case FStatus of

    mpsRecord:
    begin
    {$IFDEF SINGLETON}
      if Instance = nil then
        Instance := Self
      else
        raise Exception.Create('Can''t record multiple TMacroPlayer at the same time');
    {$ELSE}
      if Players = nil then
        Players := TList.Create;
      Players.Add(Self);
    {$ENDIF}
      ClearEvents(FEvents);
      FStrings.Clear;
      FJournal := SetWindowsHookEx(WH_GETMESSAGE, GetMessageHook, hInstance, GetCurrentThreadId());
      if FJournal = 0 then
      begin
        StopRecord;
        RaiseLastOSError;
      end;
    end;

    mpsShedule:
    begin
      ClearEvents(FEvents);
      FStrings.Clear;
    end;

    mpsPlayback:
    begin
      if FThread <> nil then
      begin
        raise Exception.Create('TMacroPlayer is already playing ?');
      end else begin
        FPlaying := FEvents;
        FPlayPos := 0;
        FThread := TPlayerThread.Create(Self);
      end;
    end;

  end;
end;

procedure TMacroPlayer.StartRecord;
begin
  Status := mpsRecord;
end;

procedure TMacroPlayer.StopRecord;
begin
  Status := mpsIdle;
end;

procedure TMacroPlayer.PlayBack;
begin
  Status := mpsPlayback;
end;

function TMacroPlayer.Playing: Boolean;
begin
  Result := Status = mpsPlayback;
end;

procedure TMacroPlayer.MoveMouse(x, y: Integer; Time: Cardinal);
var
  Event: PEvent;
  Point: TPoint;
begin
  if FStatus = mpsShedule then
  begin
    Event := NewEvent;
    Event.IType := INPUT_MOUSE;
    Event.Pt.X := X;
    Event.Pt.Y := Y;
    Event.Code := 0;
    Event.Flags := MOUSEEVENTF_MOVE;
    Event.Time := Time;
  end else begin
    Point.X := X;
    Point.Y := Y;
    ClientToScreen(GetActiveWindow, Point);
    SetCursorPos(Point.X, Point.Y);
    DoSleep(Time);
  end;
end;

procedure TMacroPlayer.ClickMouse(x, y: Integer; Button, Time: Cardinal);
var
  Event: PEvent;
  Point: TPoint;
begin
  if FStatus = mpsShedule then
  begin
    Event := NewEvent;
    Event.IType := INPUT_MOUSE;
    Event.Pt.X := X;
    Event.Pt.Y := Y;
    Event.Code := 0;
    Event.Flags := Button;
    Event.Time := Time;
  end else begin
    Point.X := X;
    Point.Y := Y;
    ClientToScreen(GetActiveWindow, Point);
    ClickScreen(Point, Button);
    DoSleep(Time);
  end;
end;

procedure TMacroPlayer.ClickScreen(const Point: TPoint; Button: Cardinal);
var
  Input: TInput;
begin
  SetCursorPos(Point.X, Point.Y);
  Input.Itype := INPUT_MOUSE;
  Input.mi.dx := (65535 * Point.X) div Screen.Width;
  Input.mi.dy := (65535 * Point.Y) div Screen.Height;
  Input.mi.mouseData := 0;
  Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or Button;
  Input.mi.time := 0;
  Input.mi.dwExtraInfo := 0;
  SendInput(1, Input, SizeOf(Input));
end;

procedure TMacroPlayer.ControlClick(Control: TControl);
var
  Point: TPoint;
begin
  Point.X := Control.Width div 2;
  Point.Y := Control.Height div 2;
  Point := Control.ClientToScreen(Point);
  ClickScreen(Point, MOUSEEVENTF_LEFTDOWN);
  ClickScreen(Point, MOUSEEVENTF_LEFTUP);
end;

procedure TMacroPlayer.PressKey(vkCode, Flags, Time: Cardinal);
var
  Event: PEvent;
  Input: TInput;
begin
  if FStatus = mpsShedule then
  begin
    Event := NewEvent;
    Event.IType := INPUT_KEYBOARD;
    Event.Code := vkCode;
    Event.Flags := Flags;
    Event.Time := Time;
  end else begin
    case vkCode of
      VK_UP,
      VK_DOWN,
      VK_LEFT,
      VK_RIGHT,
      VK_HOME,
      VK_END,
      VK_PRIOR,
      VK_NEXT,
      VK_INSERT,
      VK_DELETE:
        Flags := Flags or KEYEVENTF_EXTENDEDKEY;
    end;
    Input.Itype := INPUT_KEYBOARD;
    Input.ki.wVk := vkCode;
    Input.ki.wScan := 0;
    Input.ki.dwFlags := Flags;
    Input.ki.time := 0;
    Input.ki.dwExtraInfo := 0;
    SendInput(1, Input, SizeOf(Input));
    DoSleep(Time);
  end;
end;

procedure TMacroPlayer.Shedule;
begin
  Status := mpsShedule;
end;

procedure TMacroPlayer.ProcessEvents;
begin
  if FStatus = mpsShedule then
  begin
    Playback;
    while Playing do
      Application.ProcessMessages;
    ClearEvents(FEvents);
    FStrings.Clear;
  end;
end;

procedure TMacroPlayer.Toggle(Option: TMacroPlayerOptions);
begin
  if Option in FOptions then
    FOptions := FOptions - [Option]
  else
    FOptions := FOptions + [Option];
end;

procedure TMacroPlayer.TypeString(const Str: string; Time: Cardinal);
var
  Event: PEvent;
  Inputs: array of TInput;
  Index: Integer;
begin
  if FStatus = mpsShedule then
  begin
    Event := NewEvent;
    Event.IType := INPUT_KEYBOARD;
    if Length(Str) = 1 then
    begin
      Event.Code := Ord(Str[1]);
      Event.Flags := KEYEVENTF_UNICODE;
    end else begin
      Index := FStrings.IndexOf(Str);
      if Index < 0 then
        Event.Code := FStrings.Add(Str)
      else
        Event.Code := Index;
      Event.Flags := KEYEVENTF_STRING;
    end;
    Event.Time := Time;
  end else begin
    SetLength(Inputs, Length(Str));
    for Index := 1 to Length(Str) do
    begin
      with Inputs[Index - 1] do
      begin
        Itype := INPUT_KEYBOARD;
        ki.wVk := 0;
        ki.wScan := Ord(Str[Index]);
        ki.dwFlags := KEYEVENTF_UNICODE;
        ki.time := 0;
        ki.dwExtraInfo := 0;
      end;
    end;
    SendInput(Length(Str), Inputs[0], SizeOf(TInput));
    DoSleep(Time);
  end;
end;

end.
