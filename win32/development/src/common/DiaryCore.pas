unit DiaryCore;

{ Data & Persistence }

interface

uses
  // системные
  SysUtils,

  // данные
  BusinessObjects,
  DiaryDatabase,
  Bases,
  DiaryRoutines,
  DiaryDAO,
  DiaryLocalSource,
  DiaryWebSource,

  FoodBaseDAO,
  FoodBaseLocalDAO,
  FoodBaseWebDAO,

  DishBaseDAO,
  DishBaseLocalDAO,
  DishBaseWebDAO,

  {#}DiaryWeb,  
  AnalyzeInterface,
  DiaryAnalyze,
  AutoLog,

  // службы
  ThreadExecutor,
  InetDownload;

type
  TUpdateCheckResult = (urNoConnection, urNoUpdates, urCanUpdate);
  TItemType = (itUnknown, itFood, itDish);
  TSyncResult = (srEqual, srFirstUpdated, srSecondUpdated);

  procedure Initialize();
  procedure Finalize();

  { load/save }
  procedure LoadExpander; deprecated;
  procedure SaveExpander; deprecated;

  function IdentifyItem(const ItemName: string; out Item: TMutableItem): TItemType;

  { web }
  function DownloadFoodBaseSample: boolean;
  function DownloadDishBaseSample: boolean;
  function CheckUpdates(var Date: string): TUpdateCheckResult;

  function ExportKoofs(Plain: boolean): string;

var
  { данные }
  LocalSource: TDiaryDAO;
  WebSource: TDiaryDAO;

  Diary: TDiary;

  WebClient: TDiacompClient;
  Expander: TStringMap;

  FoodBaseLocal: TFoodBaseDAO;
  FoodBaseWeb: TFoodBaseDAO;

  DishBaseLocal: TDishBaseDAO;
  DishBaseWeb: TDishBaseDAO;

  { системное }
  Inited: boolean = False;
  WORK_FOLDER: string = ''; // рабочая директория
  AnExpDate: string; { для хранения даты обновления, загруженной при старте }

const
  { СИСТЕМНОЕ }
  ADVANCED_MODE           = True;
  PROGRAM_VERSION         = '1.09';
  PROGRAM_DATE            = '2013.09.21';
  UPDATES_CHECKING_PERIOD = 7; { дней }

  { время актуальности замера и инсулина }
  BLOOD_ACTUALITY_TIME  = 90;
  INS_ACTUALITY_TIME    = 4*60;
  SEARCH_INTERVAL       = 14; // for time left

  { Параметры подключения }
  DOWNLOAD_APP_NAME   = 'DiaryCore';
  CONNECTION_TIME_OUT = 2000;
  SETUP_FILE          = 'Update.exe';
  MAX_FOODBASE_SIZE   = 500 * 1024; // byte
  MAX_DISHBASE_SIZE   = 500 * 1024; // byte

  { URL's }
  URL_SERVER          = 'http://compensationserver.narod.ru/';
  URL_VERINFO         = URL_SERVER + 'app/VerInfo.txt';
  URL_UPDATE          = URL_SERVER + 'app/Update.exe';
  URL_MATHAN          = URL_SERVER + 'app/MathAn.dll';
  URL_BORLNDMM        = URL_SERVER + 'app/borlndmm.dll';
  URL_RESTART         = URL_SERVER + 'app/restart.util';
  URL_FOODBASE_SAMPLE = URL_SERVER + 'bases/DemoFoodBase.xml';
  URL_DISHBASE_SAMPLE = URL_SERVER + 'bases/DemoDishBase.xml';

  { Local files }

  ANALYZE_LIB_FileName = 'MathAn.dll';

  // TODO: refactor constants' names

  FOLDER_BASES        = 'Bases';

    FoodBase_Name     = 'FoodBase.xml';
    FoodBase_Name_old = 'FoodBase.txt';
    FoodBaseHash_Name = 'FoodBaseHash.txt';

    DishBase_Name     = 'DishBase.xml';
    DishBase_Name_old = 'DishBase.txt';
    DishBaseHash_Name = 'DishBaseHash.txt';

    Diary_Name        = 'Diary.txt';
    Expander_Name     = 'Expander.txt';

    FoodBase_FileName     = FOLDER_BASES + '\' + FoodBase_Name;
    FoodBaseHash_FileName = FOLDER_BASES + '\' + FoodBaseHash_Name;

    DishBase_FileName     = FOLDER_BASES + '\' + DishBase_Name;
    DishBaseHash_FileName = FOLDER_BASES + '\' + DishBaseHash_Name;

    Diary_FileName        = FOLDER_BASES + '\' + Diary_Name;
    Expander_FileName     = FOLDER_BASES + '\' + Expander_Name;

implementation

{==============================================================================}
procedure Initialize();
{==============================================================================}
begin
  AutoLog.Log(DEBUG, 'Loading local diary...');
  LocalSource := TDiaryLocalSource.Create(WORK_FOLDER + Diary_FileName);
  AutoLog.Log(DEBUG, 'Local diary loaded');

  WebClient := TDiacompClient.Create;
  WebSource := TDiaryWebSource.Create(WebClient);

  Diary := TDiary.Create(LocalSource);

  FoodBaseLocal := TFoodBaseLocalDAO.Create(WORK_FOLDER + FoodBase_FileName, WORK_FOLDER + FoodBaseHash_FileName);
  FoodBaseWeb := TFoodbaseWebDAO.Create(WebClient);

  DishBaseLocal := TDishBaseLocalDAO.Create(WORK_FOLDER + DishBase_FileName, WORK_FOLDER + DishBaseHash_FileName);
  DishBaseWeb := TDishbaseWebDAO.Create(WebClient);

  Expander := TStringMap.Create;
end;

{==============================================================================}
procedure Finalize();
{==============================================================================}
begin
  Diary.Free; // before sources finalization
  LocalSource.Free;
  WebSource.Free; // before WebClient
  WebClient.Free;

  FoodBaseLocal.Free;
  FoodBaseWeb.Free;
  DishBaseLocal.Free;
  DishBaseWeb.Free;


  Expander.Free;
end;

{==============================================================================}
function DownloadDishBaseSample(): boolean;
{==============================================================================}
begin
  Result := GetInetFile(
    DOWNLOAD_APP_NAME, URL_DISHBASE_SAMPLE,
    WORK_FOLDER + DishBase_FileName, nil, CONNECTION_TIME_OUT, MAX_DISHBASE_SIZE);
end;

{==============================================================================}
function DownloadFoodBaseSample(): boolean;
{==============================================================================}
begin
  Result := GetInetFile(
    DOWNLOAD_APP_NAME, URL_FOODBASE_SAMPLE,
    WORK_FOLDER + FoodBase_FileName, nil, CONNECTION_TIME_OUT, MAX_FOODBASE_SIZE);
end;

{==============================================================================}
function CheckUpdates(var Date: string): TUpdateCheckResult;
{==============================================================================}
{const
  TEMP = 'TEMP.TXT';
var
  f: TextFile;   }
begin
 { MessageDlg('Проверка обновлений отключена за неуплату', mtInformation, [mbNo], 0);
  Value['LastUpdateCheck'] := Round(GetTimeUTC());
  Result := urNoConnection;
  try
    if GetInetFile('Compensation', URL_VERINFO, TEMP, nil, CONNECTION_TIME_OUT, 15) then
    begin
      AssignFile(f, TEMP);
      Reset(f);
      Readln(f, Date);
      CloseFile(f);
      DeleteFile(TEMP);

      if length(Date) = 10 then
      if Date > PROGRAM_DATE then
        Result := urCanUpdate
      else
        Result := urNoUpdates;
    end;
  except
  end;  }
  Result := urNoConnection;
end;

{==============================================================================}
function IdentifyItem(const ItemName: string; out Item: TMutableItem): TItemType;
{==============================================================================}
begin
  Item := FoodBaseLocal.FindOne(ItemName);
  if (Item <> nil) then
  begin
    Result := itFood;
    Exit;
  end;

  Item := DishBaseLocal.FindOne(ItemName);
  if (Item <> nil) then
  begin
    Result := itDish;
    Exit;
  end;

  Result := itUnknown;
end;

{==============================================================================}
procedure LoadExpander;
{==============================================================================}
begin
  if FileExists(WORK_FOLDER + Expander_FileName) then
    Expander.LoadFromFile(WORK_FOLDER + Expander_FileName);
end;

{==============================================================================}
procedure SaveExpander;
{==============================================================================}
begin
  Expander.SaveToFile(WORK_FOLDER + Expander_FileName);
end;

{==============================================================================}
function ExportKoofs(Plain: boolean): string;
{==============================================================================}
var
  i: integer;
  s: string;
  DC: char;
  Koof: TKoof;
begin
  StartProc('ExportKoofs()');

  Result := '';
  if (Plain) then
  begin
    for i := 0 to MinPerDay - 1 do
    begin
      Koof := GetKoof(i);
      Result := Result +
        //Format('%2.2d',[i]) + '.00 - '+Format('%2.2d',[i + 1]) + '.00' + #9+
        Format('%.6f'#9'%.6f'#9'%.6f'#13#10, [Koof.k, Koof.q, Koof.p])
      ;
    end;
  end else
  begin
    Result := '[';
    DC := DecimalSeparator;
    try                    
      DecimalSeparator := '.';

      for i := 0 to MinPerDay - 1 do
      begin
        Koof := GetKoof(i);

        s := Format('{"time":%d,"k":%.4f,"q":%.2f,"p":%.2f}',[i, Koof.k, Koof.q, Koof.p]);
        if (i < MinPerDay - 1) then
          s := s + ',';
        Result := Result + s;
      end;
    finally
      DecimalSeparator := DC;
    end;  
    Result := Result + ']';
  end;

  FinishProc;
end;

end.
