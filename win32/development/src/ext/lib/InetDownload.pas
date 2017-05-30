unit InetDownload;

interface

uses
  SysUtils, Windows, Wininet;

type
  TCallBackProcedure = procedure;

  function GetRandomFileName(): String;
  function GetInetFile(const URL, FileName: String; MaxSize: Int64 = 0; CallBack: TCallBackProcedure = nil): boolean;
  function DoGet(const URL: String; MaxSize: Int64; CallBack: TCallBackProcedure; out Response: String): boolean;

implementation

const
  DOWNLOAD_APP_NAME             = 'DiaryCore';
  CONNECTION_TIME_OUT: Cardinal = 10000;

{======================================================================================================================}
function GetRandomFileName(): String;
{======================================================================================================================}
begin
  Result := Format('download.%d.tmp', [Random(1000000)])
end;

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
function DoGet(const URL: String; MaxSize: Int64; CallBack: TCallBackProcedure; out Response: String): boolean;
{======================================================================================================================}
var
  f: TextFile;
  TempFile: String;
begin
  Result := False;
  TempFile := GetRandomFileName();

  if GetInetFile(URL, TempFile, MaxSize, CallBack) then
  begin
    AssignFile(f, TempFile);
    Reset(f);
    Readln(f, Response);
    CloseFile(f);
    DeleteFile(PChar(TempFile));

    Result := True;
  end;
end;

initialization
  Randomize();
end.
