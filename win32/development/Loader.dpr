program Loader;

{===================================================

Downloads given URL as a file specified, creating backup for old one

Program arguments:
- 1. URL to download     - $URL
- 2. Target file name    - $TARGET

Process details:
- Downloads URL file to a temp file (deletes old one if it exists)
- Renames target file to a backup file (deletes old one if exists)
- Renames temp file to target file

===================================================}

{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  InetDownload;

  procedure Error(const Msg: String);
  begin
    writeln(Msg);
    writeln('');
    write('Press [Enter] to exit ...');
    readln;
    halt;
  end;

  function FileSize(FileName: String): Int64;
  var
    Search: TSearchRec;
  begin
    if FindFirst(FileName, faAnyFile, Search) = 0 then
      Result := Int64(Search.FindData.nFileSizeHigh) shl Int64(32) + Int64(Search.FindData.nFileSizeLow)
    else
      Result := -1;

    FindClose(Search);
  end;

const
  TEMP_FILE_NAME = 'download.tmp';
  MIN_FILE_SIZE  = 1024 * 1024;
  MAX_FILE_SIZE  = 10 * 1024 * 1024;
var
  URL: String;            // URL to file to be downloaded
  TargetFileName: String; // full path to app to be updated, e.g. "C:\Program Files\Compensation\Compensation.exe"
  TempFileName: String;   // download file name,             e.g. "C:\Program Files\Compensation\download.tmp"
  BackupFileName: String; // backup file name,               e.g. "C:\Program Files\Compensation\Compensation.exe.bak"
  ActualSize: Int64;
begin
  SetConsoleCP(1251);
  SetConsoleOutputCP(1251);

  try
    // READING ARGUMENTS

    if (ParamCount < 2) then
    begin
      Error('Expected 2 command line arguments');
    end;

    if (ParamCount > 2) then
    begin
      writeln('Found ' + IntToStr(ParamCount) + ' arguments; only first 2 will be used');
    end;

    URL := ParamStr(1);             // what to download
    TargetFileName := ParamStr(2);  // where to save (to be overriden)

    // PROCESS

    TempFileName := ExtractFilePath(TargetFileName) + TEMP_FILE_NAME;
    BackupFileName := TargetFileName + '.bak';

    writeln('UPDATER');
    writeln('- Source URL: ' + URL);
    writeln('- Target file: ' + TargetFileName);
    writeln;

    if (FileExists(TempFileName)) then
    begin
      writeln('Deleting previous download: ' + TempFileName);
      SysUtils.DeleteFile(TempFileName);

      if (not FileExists(TempFileName)) then
      begin
        writeln('Deleted OK');
      end else
      begin
        Error('Failed to delete');
      end;
    end;

    writeln('Downloading ' + URL);
    if GetInetFile(URL, TempFileName, MAX_FILE_SIZE) and
       FileExists(TempFileName) then
    begin
      ActualSize := FileSize(TempFileName);
      if (ActualSize >= MIN_FILE_SIZE) then
      begin
        writeln(Format('Loaded %d bytes OK', [ActualSize]));
      end else
      begin
        Error(Format('Target file seems corrupted, expected at least %d bytes, but loaded only %d bytes', [MIN_FILE_SIZE, ActualSize]));
      end;

      if (FileExists(BackupFileName)) then
      begin
        writeln('Deleting old backup: ' + BackupFileName);
        DeleteFile(BackupFileName);

        if (not FileExists(BackupFileName)) then
        begin
          writeln('Backup deleted OK');
        end else
        begin
          Error('Failed to delete backup file');
        end;
      end;

      if (FileExists(TargetFileName)) then
      begin
        writeln('Creating new backup: ' + BackupFileName);
        RenameFile(TargetFileName, BackupFileName);

        if (FileExists(BackupFileName)) then
        begin
          writeln('Backup created OK');
        end else
        begin
          Error('Failed to create backup');
        end;
      end;

      writeln('Renaming target file: ' + TempFileName + ' -> ' + TargetFileName);
      if (not RenameFile(TempFileName, TargetFileName)) or
         (not FileExists(TargetFileName)) then
      begin
        Error('Failed to rename');
      end else
      begin
        writeln('Done');
      end;
    end else
    begin
      Error('File corrupted (less than ' + IntToStr(MIN_FILE_SIZE) + ' bytes)');
    end;  
  except
    on e: EInOutError do
    begin
      Error('Access denied: ' + e.ClassName + ' : ' + e.Message);
    end;
    on e: Exception do
    begin
      Error('Unexpected ' + e.ClassName + ' : ' + e.Message);
    end;
  end;
end.
