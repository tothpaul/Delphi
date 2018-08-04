unit Execute.Parallelization;
{
 (c)2013 Paul TOTH <tothpaul@free.fr>
 http://lookinside.free.fr/delphi.php?Multithreading
}
interface

uses
  Windows, Classes;

type
  TIterationEvent = procedure(Sender: TObject; Index: Integer) of object;

  TParallelizer = class
  private
    FCores      : Integer;
    FTaskCount  : Integer;
    FIterator   : Integer;
    FIterations : Integer;
    FOnIteration: TIterationEvent;
    procedure ThreadProc;
  public
    constructor Create;
    procedure Iterate(Count: Integer);
    property OnIteration: TIterationEvent read FOnIteration write FOnIteration;
  end;

implementation

type
  TParallelTask = class(TThread)
  private
    FParallelizer: TParallelizer;
  public
    constructor Create(AParallelizer: TParallelizer);
    procedure Execute; override;
  end;

{ TParallelTask }

constructor TParallelTask.Create(AParallelizer: TParallelizer);
begin
  FParallelizer := AParallelizer;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TParallelTask.Execute;
begin
  FParallelizer.ThreadProc;
end;

{ TParallelizer }

constructor TParallelizer.Create;
var
  SysInfo: TSystemInfo;
begin
  inherited;
  GetSystemInfo(SysInfo);
  FCores := SysInfo.dwNumberOfProcessors;
end;

procedure TParallelizer.Iterate(Count: Integer);
var
  Index: Integer;
begin
  if not Assigned(FOnIteration) then
    Exit;
  FTaskCount := FCores;
  FIterator := -1;
  FIterations := Count;
  for Index := 1 to FCores - 1 do
  begin
    TParallelTask.Create(Self);
  end;
  ThreadProc;
  while FTaskCount > 0 do
    Sleep(0);
end;

procedure TParallelizer.ThreadProc;
var
  Index: Integer;
begin
  try
    Index := InterlockedIncrement(FIterator);
    while Index < FIterations do
    begin
      FOnIteration(Self, Index);
      Index := InterlockedIncrement(FIterator);
    end;
  finally
    InterlockedDecrement(FTaskCount);
  end;
end;

end.

