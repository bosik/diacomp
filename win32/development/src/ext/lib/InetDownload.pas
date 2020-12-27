unit InetDownload;

interface

uses
  Windows,
  WinInet;

type
  TCallBackProcedure = procedure;
  TByteArray = array of byte;
  TStringArray = array of String;

  function GetInetFile(const URL, FileName: String; MaxSize: Int64 = 0; CallBack: TCallBackProcedure = nil): boolean;
  function GetAsByteArray(const URL: String; MaxSize: Int64 = 0; CallBack: TCallBackProcedure = nil): TByteArray;
  function GetAsString(const URL: String; MaxSize: Int64 = 0; CallBack: TCallBackProcedure = nil): String;

  function ToHexArray(Bytes: TByteArray): TStringArray;

implementation

const
  DOWNLOAD_APP_NAME             = 'Diacomp';
  CONNECTION_TIME_OUT: Cardinal = 10000;

{======================================================================================================================}
function GetInetFile(const URL, FileName: String; MaxSize: Int64 = 0; CallBack: TCallBackProcedure = nil): boolean;
{======================================================================================================================}
const
  BufferSize = 1024;
var
  hSession, hURL: HInternet;
  Buffer: array[1..BufferSize] of Byte;
  BufferLen: DWORD;
  f: File;
  Count: integer;
begin
  Result := False;

  hSession := InternetOpen(PChar(DOWNLOAD_APP_NAME), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if (hSession <> nil) then
  try
    InternetSetOption(hSession, INTERNET_OPTION_CONNECT_TIMEOUT, @CONNECTION_TIME_OUT, SizeOf(Cardinal));
    hURL := InternetOpenURL(hSession, PChar(URL), nil, 0, 0, 0);
    if (hURL <> nil) then
    try
      AssignFile(f, FileName);
      Rewrite(f, 1);
      Count := 0;
      repeat
        InternetReadFile(hURL, @Buffer, SizeOf(Buffer), BufferLen);
        BlockWrite(f, Buffer, BufferLen);

        if Assigned(CallBack) then CallBack;

        inc(Count, BufferLen);
        if (MaxSize > 0)and(Count >= MaxSize) then
          Break;
      until BufferLen = 0;
      CloseFile(f);
      Result := True;
    finally
      InternetCloseHandle(hURL);
    end;
  finally
    InternetCloseHandle(hSession);
  end;
end;

{======================================================================================================================}
function GetAsByteArray(const URL: String; MaxSize: Int64 = 0; CallBack: TCallBackProcedure = nil): TByteArray;
{======================================================================================================================}
const
  BufferSize = 1024;
var
  hSession, hURL: HInternet;
  Buffer: array[1..BufferSize] of Byte;
  BufferLength: DWORD;
  Count: integer;
begin
  SetLength(Result, 0);

  hSession := InternetOpen(PChar(DOWNLOAD_APP_NAME), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if (hSession <> nil) then
  try
    InternetSetOption(hSession, INTERNET_OPTION_CONNECT_TIMEOUT, @CONNECTION_TIME_OUT, SizeOf(Cardinal));
    hURL := InternetOpenURL(hSession, PChar(URL), nil, 0, 0, 0);
    if (hURL <> nil) then
    try
      Count := 0;
      repeat
        InternetReadFile(hURL, @Buffer, SizeOf(Buffer), BufferLength);

        SetLength(Result, Length(Result) + BufferLength);
        CopyMemory(@Result[Count], @Buffer[1], BufferLength);
        inc(Count, BufferLength);

        if Assigned(CallBack) then CallBack;

        if (MaxSize > 0)and(Count >= MaxSize) then
        begin
          Break;
        end;
      until BufferLength = 0;
    finally
      InternetCloseHandle(hURL);
    end;
  finally
    InternetCloseHandle(hSession);
  end;
end;

{======================================================================================================================}
function GetAsString(const URL: String; MaxSize: Int64 = 0; CallBack: TCallBackProcedure = nil): String;
{======================================================================================================================}
var
  Data: TByteArray;
begin
  Data := GetAsByteArray(URL, MaxSize, CallBack);
  SetString(Result, PAnsiChar(@Data[0]), Length(Data));
  Result := System.Utf8ToAnsi(Result);
end;

{======================================================================================================================}
function ToHexArray(Bytes: TByteArray): TStringArray;
{======================================================================================================================}
const
  HexChars: array[0..15] of char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
var
  i: integer;
begin
  SetLength(Result, Length(Bytes));
  for i := 0 to High(Result) do
  begin
    Result[i] := HexChars[Bytes[i] div 16] + HexChars[Bytes[i] mod 16];
  end;
end;

end.
