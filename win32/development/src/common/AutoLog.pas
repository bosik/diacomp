unit AutoLog;

{$DEFINE LOGGING}

interface

uses
  Windows, Classes, SysUtils, DiaryRoutines;

type
  TLogType = (ERROR, WARNING, INFO, DEBUG, VERBOUS);

  procedure StartLogger(const Path: string);
  procedure StopLogger;

  procedure Log(MsgType: TLogType; const Msg: string; Save: boolean = False);
  procedure SaveLog;

  procedure StartProc(const Name: string);
  procedure FinishProc;
  function GetCurrentLog(): TStrings;

  function LevelToIndex(Level: TLogType): integer;
  function IndexToLevel(i: integer): TLogType;

var
  LogLevel: TLogType = INFO;
  
implementation

const
  TYPES: array[TLogType] of string = ('ERROR', 'WARNING', 'INFO', 'DEBUG', 'VERBOUS');

type
  TStackNode = record
    Time: Cardinal;
    Name: string;
  end;

var
  LogFile: TStrings = nil;
  Stack: array of TStackNode;
  StackSize: integer = 0;
  FileName: string;

  function GetCurrentLog(): TStrings;
  begin
    Result := LogFile;
  end;

{======================================================================================================================}
function LevelToIndex(Level: TLogType): integer;
{======================================================================================================================}
begin
  case Level of
    ERROR:   Result := 0;
    WARNING: Result := 1;
    INFO:    Result := 2;
    DEBUG:   Result := 3;
    VERBOUS: Result := 4;
  end;
end;

{======================================================================================================================}
function IndexToLevel(i: integer): TLogType;
{======================================================================================================================}
begin
  case i of
    0: Result := ERROR;
    1: Result := WARNING;
    2: Result := INFO;
    3: Result := DEBUG;
    4: Result := VERBOUS;
  end;
end;

{======================================================================================================================}
function CheckLevel(Level: TLogType): boolean;
{======================================================================================================================}
begin
  Result := LevelToIndex(Level) <= LevelToIndex(LogLevel);
end;

function GetLogFileName(Index: integer): String;
begin
  if (Index > 0) then
    Result := Format('diacomp.%d.log', [Index])
  else
    Result := 'diacomp.log';
end;

{======================================================================================================================}
procedure Log(MsgType: TLogType; const Msg: string; Save: boolean = False);
{======================================================================================================================}
{$IFDEF LOGGING}

  procedure RotateLogs();
  var
    i, MaxIndex: integer;
  begin
    // find oldest file index
    MaxIndex := -1;
    while (FileExists(GetLogFileName(MaxIndex + 1))) do
    begin
      inc(MaxIndex);
    end;

    // do rename
    for i := MaxIndex downto 0 do
    begin
      RenameFile(
        GetLogFileName(i),
        GetLogFileName(i + 1)
      );
    end;
  end;

var
  Date: string;
{$ENDIF}
begin
  {$IFDEF LOGGING}

  if (LogFile = nil) then
    raise Exception.CreateFmt('Failed to print "%s": logger not initialized', [Msg]);

  if (CheckLevel(MsgType)) then
  begin
    DateTimeToString(Date, 'yyyy-MM-dd hh:mm:ss.zzz', GetTimeUTC());

    LogFile.Add(Date + #9 + TYPES[MsgType] + #9 + Msg);

    //if (Save or (MsgType = ERROR)) then
    LogFile.SaveToFile(FileName);

    // TODO: optimize performance?
    if (Length(LogFile.Text) >= 10 * 1024 * 1024) then
    begin
      RotateLogs();
      LogFile.Clear();
      LogFile.SaveToFile(FileName);
    end;
  end;

  {$ENDIF}
end;

function Tabs(Count: integer): string;
var
  i: integer;
begin
  SetLength(Result, Count);
  for i := 1 to Count do
    Result[i] := #9;
end;

{======================================================================================================================}
procedure StartProc(const Name: string);
{======================================================================================================================}
begin
  {$IFDEF LOGGING}
  if (StackSize = Length(Stack)) then
    SetLength(Stack, Length(Stack)*2 + 1);

  inc(StackSize);

  Stack[StackSize - 1].Name := Name;
  Stack[StackSize - 1].Time := GetTickCount;

  Log(VERBOUS, Tabs(StackSize) + '<' + Name + '>');
  {$ENDIF}
end;

{======================================================================================================================}
procedure FinishProc();
{======================================================================================================================}
begin
  {$IFDEF LOGGING}
  if (StackSize > 0) then
  begin
    Log(VERBOUS, Tabs(StackSize) + '</' + Stack[StackSize - 1].Name + '> (' + IntToStr(GetTickCount - Stack[StackSize - 1].Time) + ')');
    dec(StackSize);
  end;
  {$ENDIF}
end;

{======================================================================================================================}
procedure SaveLog();
{======================================================================================================================}
begin
  {$IFDEF LOGGING}
  LogFile.SaveToFile(FileName);
  {$ENDIF}
end;

{======================================================================================================================}
procedure StartLogger(const Path: string);
{======================================================================================================================}
begin
  {$IFDEF LOGGING}

  if not DirectoryExists(Path) then
    CreateDirectory(PChar(Path), nil);

  FileName := Path + '\' + GetLogFileName(0);

  LogFile := TStringList.Create();

  if (FileExists(FileName)) then
    LogFile.LoadFromFile(FileName);

  {$ENDIF}
end;

{======================================================================================================================}
procedure StopLogger();
{======================================================================================================================}
begin
end;

end.
