unit AutoLog;

{$DEFINE LOGGING}

interface

uses
  Windows, Classes, SysUtils;

type
  TLogType = (ERROR, WARNING, INFO, DEBUG, VERBOUS);

  procedure StartLogger;
  procedure StopLogger;

  procedure Log(MsgType: TLogType; const Msg: string; Save: boolean = False);
  procedure SaveLog;

  procedure StartProc(const Name: string);
  procedure FinishProc;

implementation

{$IFDEF LOGGING}

type
  TStackNode = record
    Time: Cardinal;
    Name: string;
  end;

const
  FOLDER_LOGS = 'Logs';

var
  LogFile: TStrings;
  Stack: array of TStackNode;
  StackSize: integer = 0;
  FileName: string;  

{$ENDIF}

{==============================================================================}
procedure Log(MsgType: TLogType; const Msg: string; Save: boolean = False);
{==============================================================================}
{$IFDEF LOGGING}
var
  Temp: string;
{$ENDIF}
begin
  {$IFDEF LOGGING}

  DateTimeToString(Temp, 'hh:mm:ss.zzz', Now);

  case MsgType of
    ERROR:   Temp := Temp + #9'ERROR'#9;
    WARNING: Temp := Temp + #9'WARNING'#9;
    INFO:    Temp := Temp + #9'INFO'#9;
    DEBUG:   Temp := Temp + #9'DEBUG'#9;
    VERBOUS: Temp := Temp + #9'VERBOUS'#9;
  end;

  if Save then
    Temp := Temp + '! ';
  Temp := Temp + Msg;

  LogFile.Add(Temp);

  if Save then
    LogFile.SaveToFile(FileName);

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

{==============================================================================}
procedure StartProc(const Name: string);
{==============================================================================}
begin
  if (StackSize = Length(Stack)) then
    SetLength(Stack, Length(Stack)*2 + 1);

  inc(StackSize);

  Stack[StackSize - 1].Name := Name;
  Stack[StackSize - 1].Time := GetTickCount;

  Log(VERBOUS, Tabs(StackSize) + '<' + Name + '>');
end;

{==============================================================================}
procedure FinishProc;
{==============================================================================}
begin
  if (StackSize > 0) then
  begin
    Log(VERBOUS, Tabs(StackSize) + '</' + Stack[StackSize - 1].Name + '> (' + IntToStr(GetTickCount - Stack[StackSize - 1].Time) + ')');
    dec(StackSize);
  end;
end;

{==============================================================================}
procedure SaveLog;
{==============================================================================}
begin
  {$IFDEF LOGGING}
  LogFile.SaveToFile(FileName);
  {$ENDIF}
end;

{==============================================================================}
procedure StartLogger;
{==============================================================================}
begin
  {$IFDEF LOGGING}

  if not DirectoryExists(FOLDER_LOGS) then
    CreateDirectory(PChar(FOLDER_LOGS), nil);

  DateTimeToString(FileName, 'yyyy-mm-dd_hh-mm-ss', Now);
  FileName := FOLDER_LOGS + '\' + FileName + '.txt';

  LogFile := TStringList.Create;

  {$ENDIF}
end;

{==============================================================================}
procedure StopLogger;
{==============================================================================}
begin
  {$IFDEF LOGGING}
  LogFile.Free;
  {$ENDIF}
end;

end.
