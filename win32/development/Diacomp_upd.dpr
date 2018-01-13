program Diacomp_upd;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  
  InetDownload,
  DiaryRoutines;

const
  URL_SERVER      = 'http://diacomp.net/';
  URL_VERSION     = URL_SERVER + 'api/windows/version';
  URL_APP         = URL_SERVER + 'api/windows/file/compensation.exe';

  function GetLatestVersion(): integer;
  var
    Response: String;
  begin
    if (DoGet(URL_VERSION, 1024, nil, Response) and (Response <> '') and TryStrToInt(Response, Result)) then
    begin
      // ok
    end else
      Result := -1;
  end;

  procedure Error(const msg: String);
  begin
    writeln('[ERROR] ' + msg);
    writeln('');
    writeln('Press [Enter] to exit ...');
    readln;
    halt;
  end;

var
  AppFileName: String;
  TempFileName: String;
  Path: String;
  OldVersion: integer;
  NewVersion: integer;
begin
  try
    // READING ARGUMENTS

    if (ParamCount < 2) then
    begin
      Error('Command line arguments required');
    end;

    AppFileName := ParamStr(1);
    OldVersion := StrToInt(ParamStr(2));

    // VALIDATING ARGUMENTS

    if (not FileExists(AppFileName)) then
    begin
      Error('Application file not found: ' + AppFileName);
    end;

    // PERFORM

    Path := ExtractFilePath(AppFileName);
    AppFileName := ExtractFileName(AppFileName);
    TempFileName := GetRandomFileName();

    writeln('Updating ' + Path);
    writeln('Current version: ', OldVersion);

    writeln('Checking latest version...');
    NewVersion := GetLatestVersion();
    writeln('Latest version: ', NewVersion);

    if (OldVersion >= NewVersion) then
    begin
      writeln('Software is up to date');
    end else
    begin
      writeln('Downloading app...');

      if GetInetFile(URL_APP, Path + TempFileName, 10 * 1024 * 1024) and
         FileExists(Path + TempFileName) and
         (FileSize(Path + TempFileName) > 1024 * 1024) then
      begin
        writeln('Removing old backup...');
        DeleteFile(Path + AppFileName + '.bak');

        writeln('Creating new backup...');
        RenameFile(Path + AppFileName, Path + AppFileName + '.bak');

        writeln('Preparing app...');
        RenameFile(Path + TempFileName, Path + AppFileName);

        writeln('App successfully updated');
      end else
        Error('App file corrupted (less than 1 Mb)');
    end;

    // Remove itself
    Path := ParamStr(0);
    writeln('Deleting ' + Path);
    DeleteFile(Path);
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
