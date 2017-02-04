unit ThreadExecutor;

interface

uses
  Classes, Windows;

type
  //TTask = procedure();   
  TBooleanFunction = function(): Boolean of object; 
  TTerminableTask = procedure(Terminated: TBooleanFunction);
  TThreadNotifier = procedure(ThreadID: cardinal) of object;
  TCmpThreadNotifier = procedure(Sender: TObject; TaskID: cardinal) of object;

  TMyThread = class(TThread)
  private
    FProc: TTerminableTask;
    function IsTerminated_: Boolean;
  protected
    procedure Execute; override;
  public
    procedure Start(P: TTerminableTask; APriority: TThreadPriority);
  end;

  TTimeoutThread = class(TThread)
  private
    FThread: TMyThread;
    FStartTime: cardinal;
    FTimeOut: cardinal;
    FOnDone: TThreadNotifier;
    FOnTimeOut: TThreadNotifier;
  protected
    procedure Execute; override;
    procedure Done;
    procedure Timeout;
  public
    procedure Control(AThread: TMyThread; Timeout: cardinal);
    property OnDone: TThreadNotifier read FOnDone write FOnDone;
    property OnTimeOut: TThreadNotifier read FOnTimeOut write FOnTimeOut;
  end;

  TTaskItem = record
    TaskID: cardinal;
    ThreadID: cardinal;
  end;


  {

  ВНИМАНИЕ!
  Выполняемые задачи не должны обращаться к GUI приложения.
  Однако события OnDone и OnTimeout вызываются через Synchronize
  и могут выводить любые данные.

  }
  
  TThreadExecutor = class (TComponent)
  private
    FOnDone: TCmpThreadNotifier;
    FOnTimeOut: TCmpThreadNotifier;
    FTaskList: array of TTaskItem;
  protected
    procedure AddTask(ATaskID, AThreadID: cardinal);
    function GetTaskID(ThreadID: cardinal): cardinal;
    procedure MyDone(ThreadID: cardinal);
    procedure MyTimeOut(ThreadID: cardinal);
  public
    procedure Execute(P: TTerminableTask; Priority: TThreadPriority; Timeout: cardinal;
      TaskID: cardinal = 0);
  published
    property OnDone: TCmpThreadNotifier read FOnDone write FOnDone;
    property OnTimeOut: TCmpThreadNotifier read FOnTimeOut write FOnTimeOut;
  end;

  function ExecuteInThread(P: TTerminableTask; Priority: TThreadPriority; Timeout: cardinal;
    OnDone: TThreadNotifier = nil; OnTimeOut: TThreadNotifier = nil): cardinal;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Win32', [TThreadExecutor]);
end;

{ TMyThread }

procedure TMyThread.Execute;
begin
  if Assigned(FProc) then
    FProc(IsTerminated_);
end;

function TMyThread.IsTerminated_: Boolean;
begin
  Result := Terminated;
end;

procedure TMyThread.Start(P: TTerminableTask; APriority: TThreadPriority);
begin
  FreeOnTerminate := True; // ***
  FProc := P;
  Priority := APriority;
  Resume;
end;

{ TTimeoutThread }

procedure TTimeoutThread.Control(AThread: TMyThread; Timeout: cardinal);
begin
  FThread := AThread;
  FStartTime := GetTickCount;
  FTimeOut := TimeOut;
  Priority := tpLowest;
  FreeOnTerminate := True; // ***
  Resume;  
end;

procedure TTimeoutThread.Done;
begin
  if Assigned(FOnDone) then
    FOnDone(FThread.ThreadID);
end;

procedure TTimeoutThread.Execute;
begin
  if (WaitForSingleObject(FThread.Handle, FTimeOut) = WAIT_TIMEOUT) then
  begin
    // время вышло, поток всё ещё работает
    FThread.Suspend;
    TerminateProcess(FThread.Handle, 0);

    Synchronize(Self.Timeout);
    //FThread.Free;
  end else
    // поток успел завершиться
    Synchronize(Done);
end;

procedure TTimeoutThread.Timeout;
begin
  if Assigned(FOnTimeOut) then
    FOnTimeOut(FThread.ThreadID);
end;

{==============================================================================}
function ExecuteInThread(P: TTerminableTask; Priority: TThreadPriority;
  Timeout: cardinal; OnDone: TThreadNotifier; OnTimeOut: TThreadNotifier): cardinal;
{==============================================================================}
var
  T: TMyThread;
  C: TTimeoutThread;
begin
  // подготовка
  T := TMyThread.Create(True);
  C := TTimeoutThread.Create(True);
  C.OnDone := OnDone;
  C.OnTimeOut := OnTimeout;
  Result := T.ThreadID;

  // запуск 
  T.Start(P, Priority);
  C.Control(T, Timeout);

  // Утечка? Пофиг.
end;

{ TThreadExecutor }

procedure TThreadExecutor.AddTask(ATaskID, AThreadID: cardinal);
begin
  SetLength(FTaskList, Length(FTaskList) + 1);
  with FTaskList[high(FTaskList)] do
  begin
    TaskID := ATaskID;
    ThreadID := AThreadID;
  end;
end;

procedure TThreadExecutor.Execute(P: TTerminableTask; Priority: TThreadPriority;
  Timeout: cardinal; TaskID: cardinal);
var
  ThreadID: integer;
begin
  ThreadID := ExecuteInThread(P, Priority, Timeout, MyDone, MyTimeout);
  AddTask(TaskID, ThreadID);
end;

function TThreadExecutor.GetTaskID(ThreadID: cardinal): cardinal;
var
  i,n: integer;
begin
  // при нахождении удаляет
  for i := 0 to high(FTaskList) do
  if FTaskList[i].ThreadID = ThreadID then
  begin
    Result := FTaskList[i].TaskID;   
    n := high(FTaskList);
    FTaskList[i] := FTaskList[n];
    SetLength(FTaskList, n);
    Exit;
  end;
  Result := 0;
end;

procedure TThreadExecutor.MyDone(ThreadID: cardinal);
begin
  if Assigned(FOnDone) then
    FOnDone(Self, GetTaskID(ThreadID));
end;

procedure TThreadExecutor.MyTimeOut(ThreadID: cardinal);
begin
  if Assigned(FOnTimeOut) then
    FOnTimeOut(Self, GetTaskID(ThreadID));
end;

end.
