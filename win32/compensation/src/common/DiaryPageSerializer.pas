unit DiaryPageSerializer;

interface

uses
  SysUtils,
  Classes,
  Dialogs, // TODO: debug only
  AutoLog, // TODO: debug obly
  DiaryRoutines;

type
  TPageData = class; // forward

  TPageDataList = array of TPageData;

  // нераспарсенная страница
  TPageData = class
    Date: TDate;
    TimeStamp: TDateTime;
    Version: integer;
    Page: string;

    procedure CopyFrom(ADate: TDate; ATimeStamp: TDateTime; AVersion: integer; const APage: string); overload;
    procedure CopyFrom(APage: TPageData); overload;
    constructor Create(ADate: TDate; ATimeStamp: TDateTime; AVersion: integer; const APage: string); overload;
    constructor Create(APage: TPageData); overload;

    procedure Read(const S: string; WebFormat: boolean);
    procedure ReadHeader(const S: string; WebFormat: boolean = False);
    function Write(WebFormat: boolean): string;
    function WriteHeader(WebFormat: boolean): string;

    class procedure MultiRead(S: TStrings; WebFormat: boolean; out Pages: TPageDataList);
    class procedure MultiWrite(S: TStrings; WebFormat: boolean; const Pages: TPageDataList);
  end;

implementation

{ TPageData }

{==============================================================================}
procedure TPageData.CopyFrom(ADate: TDate; ATimeStamp: TDateTime;
  AVersion: integer; const APage: string);
{==============================================================================}
begin
  Date := ADate;
  TimeStamp := ATimeStamp;
  Version := AVersion;
  Page := APage;
end;

{==============================================================================}
procedure TPageData.CopyFrom(APage: TPageData);
{==============================================================================}
begin
  CopyFrom(APage.Date, APage.TimeStamp, APage.Version, APage.Page);
end;

{==============================================================================}
constructor TPageData.Create(ADate: TDate; ATimeStamp: TDateTime;
  AVersion: integer; const APage: string);
{==============================================================================}
begin
  CopyFrom(ADate, ATimeStamp, AVersion, APage);
end;

{==============================================================================}
constructor TPageData.Create(APage: TPageData);
{==============================================================================}
begin
  CopyFrom(APage);
end;

{==============================================================================}
class procedure TPageData.MultiRead(S: TStrings; WebFormat: boolean; out Pages: TPageDataList);
{==============================================================================}
var
  buf: string;
  i, Count: integer;

  procedure FlushBuffer;
  begin
    if (Trim(buf) <> '') then
    begin
      if (Length(Pages) = Count) then
        SetLength(Pages, Length(Pages) * 2 + 1);

      Pages[Count] := TPageData.Create;
      Pages[Count].Read(buf, WebFormat);
      inc(Count);

      buf := '';
    end;
  end;

begin
  buf := '';
  Count := 0;
  SetLength(Pages, 1);
  try
    for i := 0 to s.Count - 1 do
    if (s[i] <> '') then
    begin
      if (s[i][1] = '=') then FlushBuffer;
      buf := buf + s[i] + #13;
    end;
    FlushBuffer;
  finally
    SetLength(Pages, Count);
  end;
end;

{==============================================================================}
class procedure TPageData.MultiWrite(S: TStrings; WebFormat: boolean; const Pages: TPageDataList);
{==============================================================================}
var
  i: integer;
begin
  // пустые страницы могут появляться после интенсивного тыкания по календарю,
  // поэтому сохраняем только страницы с записями - МОЛОДЕЦ! :-)
  // ---
  // No. С таким подходом я не смогу перезаписать страницу на пустую. Пустое содержание - тоже содержание
  // ---
  /// Yes. Проверять нужно номер версии, а не содержание.

  s.Clear;
  for i := 0 to High(Pages) do
  // (Trim(Pages[i].Page) <> '') then
  if (Pages[i].Version > 0) then
      s.Add(Pages[i].Write(WebFormat));
end;

{==============================================================================}
procedure TPageData.Read(const S: string; WebFormat: boolean);
{==============================================================================}
var
  header, body: string;
begin
  Separate(S, header, #13, body);
  ReadHeader(header, WebFormat);
  Page := body;
end;

{==============================================================================}
procedure TPageData.ReadHeader(const S: string; WebFormat: boolean);
{==============================================================================}
var
  F: TFormatSettings;
  s1, s2, tmp: string;
begin
  try
    F := GetDateTimeFormat(WebFormat);
    Date := Trunc(StrToDate(Copy(S, 5, 10), F)); // ***
    Separate(S, s1, '|', s2);
    tmp := s2;
    Separate(tmp, s1, '|', s2);

    TimeStamp := StrToDateTime(s1, F);           // ***
    Version := StrToInt(s2);                     // ***
  except
    on E: Exception do
    begin
      Log(
        ERROR, 
        'TPageData.ReadHeader(): EXCEPTION! ' +  E.Message + #13 +
        'TPageData.ReadHeader(): S = "' + S + '"' + #13 +
        'TPageData.ReadHeader(): Copy(S, 5, 10) = "' + Copy(S, 5, 10) + '"' + #13 +
        'TPageData.ReadHeader(): tmp = "' + tmp + '"' + #13 + 
        'TPageData.ReadHeader(): TimeStamp = "' + s1 + '"' + #13 +
        'TPageData.ReadHeader(): Version = "' + s2 + '"', True);
      ShowMessage('Exception in TPageData.ReadHeader(). Check the log file for details.');
    end;    
  end;

  {
  Date := Trunc(StrToDate(Copy(s[i], 5, 10)));

  tmp := s[i];
  Separate(tmp, s1, '|', s2);

  // только дата
  if s2 = '' then
  begin
    TimeStamp := Date + 23/24;  // модификация в 23:00
    Version := 1;
  end else
  begin
    tmp := s2;
    Separate(tmp, s1, '|', s2);

    TimeStamp := StrToDateTime(s1);
    // Дата | TimeStamp
    if s2 = '' then
      Version := 1
    else
      // Дата | TimeStamp | Версия
      Version := StrToInt(s2);
  end;}
end;

{==============================================================================}
function TPageData.Write(WebFormat: boolean): string;
{==============================================================================}
begin
  Result := WriteHeader(WebFormat) + #13 + Page;
end;

{==============================================================================}
function TPageData.WriteHeader(WebFormat: boolean): string;
{==============================================================================}
var
  F: TFormatSettings;
begin
  F := GetDateTimeFormat(WebFormat);
  Result := Format('=== %s ===|%s|%d', [DateToStr(Date, F), DateTimeToStr(TimeStamp, F), Version]);

   { '=== ' + DateToStr(Date, F) + ' ===|' +
    DateTimeToStr(TimeStamp, F) + '|' +
    IntToStr(Version);    }
end;

end.
