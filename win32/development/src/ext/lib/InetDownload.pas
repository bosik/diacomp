unit InetDownload;

interface

uses
  Windows, Wininet;

type
  TCallBackProcedure = procedure;

  function GetInetFile (const sAppName,fileURL, FileName: String;
    CallBack: TCallBackProcedure; TimeOut: cardinal; MaxSize: Int64): boolean;

implementation

{==============================================================================}
function GetInetFile (const sAppName,fileURL, FileName: String;
  CallBack: TCallBackProcedure; TimeOut: cardinal; MaxSize: Int64): boolean;
{==============================================================================}
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

  hSession := InternetOpen(PChar(sAppName), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if (hSession <> nil) then
  try
    InternetSetOption(hSession, INTERNET_OPTION_CONNECT_TIMEOUT, @TimeOut, SizeOf(Cardinal));
    hURL := InternetOpenURL(hSession, PChar(fileURL), nil, 0, 0, 0);
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

end.
