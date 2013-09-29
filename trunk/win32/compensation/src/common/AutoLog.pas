unit AutoLog;

{$DEFINE LOGGING}

interface

uses
  Windows, Classes, SysUtils;

  procedure Log(const Msg: string; Save: boolean = False);
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
  LogFile: TStringList;
  Stack: array of TStackNode;
  StackSize: integer = 0;
  FileName: string;  

{$ENDIF}

{==============================================================================}
procedure Log(const Msg: string; Save: boolean = False);
{==============================================================================}
{$IFDEF LOGGING}
var
  TempTime: string;
{$ENDIF}
begin
  {$IFDEF LOGGING}

  DateTimeToString(TempTime, 'hh:mm:ss.zzz', Now);

  if not Save then
    LogFile.Add(TempTime + #9 + Msg)
  else
    LogFile.Add(TempTime + #9 + '! ' + Msg);

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

  Log(Tabs(StackSize) + '<' + Name + '>');
end;

{==============================================================================}
procedure FinishProc;
{==============================================================================}
begin
  if (StackSize > 0) then
  begin
    Log(Tabs(StackSize) + '</' + Stack[StackSize - 1].Name + '> (' + IntToStr(GetTickCount - Stack[StackSize - 1].Time) + ')');
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

initialization
  StartLogger;
finalization
  StopLogger;
end.
