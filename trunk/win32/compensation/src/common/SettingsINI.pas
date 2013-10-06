unit SettingsINI;

interface

uses
  INIFiles, SysUtils, Classes, DiaryRoutines {CheckDot()}, Forms, AutoLog;

type
  TRecordType = (rtInteger, rtFloat, rtBoolean, rtString);

  TSettingsRecord = class
    Name: string;
    RecType: TRecordType;
    Value: Variant;
    Default: Variant;
    INIPart: string;
  end;

  EIncorrectRecordName = class(Exception);

  TSettings = class
  private
    FData: array of TSettingsRecord;
  protected
    function Getter(Index: integer): Variant;
    function More(Index1, Index2: integer): boolean;
    procedure Swap(Index1, Index2: integer);

    function FindRecord(const Name: string): TSettingsRecord;
    function GetValue(const Name: string): Variant;
    procedure SetValue(const Name: string; Value: Variant);     
    procedure Sort();
  public
    procedure Add(const INIPart, Name: string; RecType: TRecordType;
      Default: Variant);
    procedure Clear;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
    procedure ResetToDefaults;
    procedure SaveToFile(const FileName: string);

    property Value[const Name: string]: Variant read GetValue write SetValue; default;
    property Rec[const Name: string]: TSettingsRecord read FindRecord;
  end;

  procedure LoadSettings;
  procedure SaveSettings;

var
  // singleton
  Value: TSettings;

implementation

const
  SETTINGS_FILENAME = 'Settings.ini';

{==============================================================================}
procedure LoadSettings;
{==============================================================================}
begin
  Value.LoadFromFile(ExtractFilePath(ParamStr(0)) + SETTINGS_FILENAME);
end;

{==============================================================================}
procedure SaveSettings;
{==============================================================================}
begin
  Value.SaveToFile(ExtractFilePath(ParamStr(0)) + SETTINGS_FILENAME);
end;

{ TSettings }

{==============================================================================}
procedure TSettings.Add(const INIPart, Name: string; RecType: TRecordType;
  Default: Variant);
{==============================================================================}
var
  n: integer;
begin
  n := Length(FData);
  SetLength(FData, n + 1);
  FData[n] := TSettingsRecord.Create;
  FData[n].Name := Name;
  FData[n].RecType := RecType;
  FData[n].Default := Default;
  FData[n].INIPart := INIPart;
end;

{==============================================================================}
procedure TSettings.Clear;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to High(FData) do
    FData[i].Free;
  SetLength(FData, 0);
end;

{==============================================================================}
destructor TSettings.Destroy;
{==============================================================================}
begin
  Clear;
end;

{==============================================================================}
function TSettings.FindRecord(const Name: string): TSettingsRecord;
{==============================================================================}
var
  i: integer;
begin
  //Log('TSettings.FindRecord("' + Name + '")');

  i := BinarySearch(Name, 0, High(FData), Getter);

  if (i <> -1) then
    Result := FData[i]
  else
  begin
    // TODO: either throw or log (antipattern #1)
    Log('ERROR: TSettings.FindRecord(): настройка"' + Name + '" отсутствует', True);
    raise EIncorrectRecordName.Create('Настройка "' + Name + '" отсутствует');
  end;
end;

{==============================================================================}
function TSettings.Getter(Index: integer): Variant;
{==============================================================================}
begin
  Result := FData[Index].Name;
end;

{==============================================================================}
function TSettings.GetValue(const Name: string): Variant;
{==============================================================================}
begin
  Result := FindRecord(Name).Value;
end;

{==============================================================================}
procedure TSettings.LoadFromFile(const FileName: string);
{==============================================================================}

  procedure CheckFileDots(const FileName: string);
  var
    s: TStrings;
    i, n: integer;
    val: string;
  begin
    s := TStringList.Create;
    try
      s.LoadFromFile(FileName);
      for i := 0 to s.Count - 1 do
      begin
        n := pos('=', s[i]);
        if n > 0 then
        begin
          val := Copy(s[i], n + 1, Length(s[i]) - n);
          if (pos('.',val)>0)or(pos(',',val)>0) then
          begin
            val := CheckDot(val);
            if TestStrToFloat(val) then
              s[i] := Copy(s[i], 1, n) + val;
          end;
        end;
      end;
      s.SaveToFile(FileName);
    finally
      s.Free;
    end;
  end;

var
  Ini: TIniFile;
  i: integer;
begin
  if FileExists(FileName) then
  try
    CheckFileDots(FileName);

    Ini := TIniFile.Create(FileName);
    try
      for i := 0 to High(FData) do
      case FData[i].RecType of
        rtInteger:  FData[i].Value := Ini.ReadInteger(FData[i].INIPart, FData[i].Name, FData[i].Default);
        rtFloat:    FData[i].Value := Ini.ReadFloat  (FData[i].INIPart, FData[i].Name, FData[i].Default);
        rtBoolean:  FData[i].Value := Ini.ReadBool   (FData[i].INIPart, FData[i].Name, FData[i].Default);
        rtString:   FData[i].Value := Ini.ReadString (FData[i].INIPart, FData[i].Name, FData[i].Default);
      end;
    finally
      Ini.Free;
    end;
  except
    // TODO: записать в лог
    ResetToDefaults;
  end else
    ResetToDefaults;
end;

{==============================================================================}
function TSettings.More(Index1, Index2: integer): boolean;
{==============================================================================}
begin
  Result := FData[Index1].Name > FData[Index2].Name;
end;

{==============================================================================}
procedure TSettings.ResetToDefaults;
{==============================================================================}
var
  i: integer;
begin
  for i := 0 to High(FData) do
    FData[i].Value := FData[i].Default;

  // классно, правда?
end;

{==============================================================================}
procedure TSettings.SaveToFile(const FileName: string);
{==============================================================================}
var
  Ini: TIniFile;
  i: integer;
begin
  try
    Ini := TIniFile.Create(FileName);
    try
      for i := 0 to High(FData) do
      case FData[i].RecType of
        rtInteger:  Ini.WriteInteger(FData[i].INIPart, FData[i].Name, FData[i].Value);
        rtFloat:    Ini.WriteFloat  (FData[i].INIPart, FData[i].Name, FData[i].Value);
        rtBoolean:  Ini.WriteBool   (FData[i].INIPart, FData[i].Name, FData[i].Value);
        rtString:   Ini.WriteString (FData[i].INIPart, FData[i].Name, FData[i].Value);
      end;
    finally
      Ini.Free;
    end;
  except
    // TODO: написать обработчик
  end;
end;

{==============================================================================}
procedure TSettings.SetValue(const Name: string; Value: Variant);
{==============================================================================}
begin
  FindRecord(Name).Value := Value;
end;

{==============================================================================}
procedure Setup;
{==============================================================================}
const
  Part_PrivateSettings   = 'ЛИЧНЫЕ';
  Part_AnalyzeSettings   = 'АНАЛИЗ';
  Part_InterfaceSettings = 'ИНТЕРФЕЙС';
  Part_Colors            = 'ЦВЕТА';
  Part_BaseCols          = 'СТОЛБЦЫ';
  Part_Export            = 'ЭКСПОРТ';
  Part_Internet          = 'ИНТЕРНЕТ';
begin
  // TODO: сделать загрузку из XML
  // TODO: там же хранить дефолты?
  with Value do
  begin
    Clear;
    
    Add(Part_PrivateSettings, 'FirstStart',     rtBoolean, True);
    Add(Part_PrivateSettings, 'UpdatedVersion', rtString,  '');
    Add(Part_PrivateSettings, 'TargetBS',       rtFloat,   5.0);
    Add(Part_PrivateSettings, 'MaxDose',        rtInteger, 10);
    Add(Part_PrivateSettings, 'BS1',            rtFloat,   3.7);
    Add(Part_PrivateSettings, 'BS2',            rtFloat,   6.2);
    Add(Part_PrivateSettings, 'BS3',            rtFloat,   7.8);
    Add(Part_PrivateSettings, 'Balance',        rtBoolean, True);
    Add(Part_PrivateSettings, 'NormProts',      rtInteger, 120);
    Add(Part_PrivateSettings, 'NormFats',       rtInteger, 90);
    Add(Part_PrivateSettings, 'NormCarbs',      rtInteger, 350);

    Add(Part_AnalyzeSettings, 'DaysProcess',        rtInteger, 30);
    Add(Part_AnalyzeSettings, 'Adaptation',         rtFloat,   0.75);
    Add(Part_AnalyzeSettings, 'UpdateKMode',        rtInteger, 1);
    Add(Part_AnalyzeSettings, 'PostPrandTime',      rtInteger, 210);
    Add(Part_AnalyzeSettings, 'ShortPostPrandTime', rtInteger, 20);
    Add(Part_AnalyzeSettings, 'ShowPoints',         rtBoolean, False);

    Add(Part_InterfaceSettings, 'DiaryFoodWidth',     rtInteger, 300);
    Add(Part_InterfaceSettings, 'PanelBaseFood.Width',rtFloat,   0.45);
    Add(Part_InterfaceSettings, 'FontSize',           rtInteger, 10);
    Add(Part_InterfaceSettings, 'FontName',           rtString,  'Arial');
    Add(Part_InterfaceSettings, 'LastReservePath',    rtString,  'C:\Compensation Reserve\|');
    Add(Part_InterfaceSettings, 'ProcShow',           rtInteger, 3);
    Add(Part_InterfaceSettings, 'DoubleBuffered',     rtBoolean, True);
    Add(Part_InterfaceSettings, 'ShadowEditors',      rtBoolean, True);
    Add(Part_InterfaceSettings, 'AnimatePanels',      rtBoolean, True);
    Add(Part_InterfaceSettings, 'AnUsingPeriod',      rtInteger, 30);
    Add(Part_InterfaceSettings, 'DishModFactor',      rtInteger, 4);
    Add(Part_InterfaceSettings, 'OptimizeBases',      rtBoolean, True);
    Add(Part_InterfaceSettings, 'MinToTray',          rtBoolean, False);
    Add(Part_InterfaceSettings, 'MealNotifies',       rtBoolean, True);
    Add(Part_InterfaceSettings, 'ColoredEditors',     rtBoolean, True);
    Add(Part_InterfaceSettings, 'CheckUpdates',       rtBoolean, True);
    Add(Part_InterfaceSettings, 'LastUpdateCheck',    rtInteger, 0);
    Add(Part_InterfaceSettings, 'CarbsInfo',          rtBoolean, True);

    Add(Part_Colors, 'Color_StdBlood',   rtInteger, $FFEEE6);
    Add(Part_Colors, 'Color_StdIns',     rtInteger, $FFFFFF);
    Add(Part_Colors, 'Color_StdMeal',    rtInteger, $DDFFFF);
    Add(Part_Colors, 'Color_StdNote',    rtInteger, $E4FFD8);
    Add(Part_Colors, 'Color_StdBloodPP', rtInteger, $F8E4F0);
    Add(Part_Colors, 'Color_SelBlood',   rtInteger, $F7DDCC);
    Add(Part_Colors, 'Color_SelIns',     rtInteger, $F0F0F0);
    Add(Part_Colors, 'Color_SelMeal',    rtInteger, $99FFFF);
    Add(Part_Colors, 'Color_SelNote',    rtInteger, $CAFFB3);
    Add(Part_Colors, 'Color_SelBloodPP', rtInteger, $F2CEE6);

    Add(Part_BaseCols, 'FoodP',  rtBoolean, True);
    Add(Part_BaseCols, 'FoodF',  rtBoolean, True);
    Add(Part_BaseCols, 'FoodC',  rtBoolean, True);
    Add(Part_BaseCols, 'FoodV',  rtBoolean, True);
    //Add(Part_BaseCols, 'FoodGI', rtBoolean, False);
    Add(Part_BaseCols, 'DishM',  rtBoolean, False);
    Add(Part_BaseCols, 'DishP',  rtBoolean, True);
    Add(Part_BaseCols, 'DishF',  rtBoolean, True);
    Add(Part_BaseCols, 'DishC',  rtBoolean, True);
    Add(Part_BaseCols, 'DishV',  rtBoolean, False);
    Add(Part_BaseCols, 'DishD',  rtBoolean, True);

    Add(Part_Export, 'Export_AddBlood', rtBoolean, True);
    Add(Part_Export, 'Export_AddIns',   rtBoolean, True);
    Add(Part_Export, 'Export_AddMeal',  rtBoolean, True);
    Add(Part_Export, 'Export_AddNote',  rtBoolean, True);
    Add(Part_Export, 'Export_ExtMeal',  rtBoolean, False);
    Add(Part_Export, 'Export_MealMode', rtInteger, 1);
    Add(Part_Export, 'Export_Breaks',   rtBoolean, True);

    Add(Part_Internet, 'AutoSync',         rtBoolean, False);
    Add(Part_Internet, 'AutosaveInterval', rtInteger, 5);
    Add(Part_Internet, 'Login',            rtString,  '');
    Add(Part_Internet, 'Password',         rtString,  '');
    Add(Part_Internet, 'ServerURL',        rtString,  'http://diacomp.16mb.com/');
    Add(Part_Internet, 'LastSync',         rtString,  '0');

    //Add(Part_, '', rt, );
  end;

  Value.Sort;
end;

{==============================================================================}
procedure TSettings.Sort;
{==============================================================================}
begin
  QuickSort(0, High(FData), Swap, More);
end;

{==============================================================================}
procedure TSettings.Swap(Index1, Index2: integer);
{==============================================================================}
var
  Temp: TSettingsRecord;
begin
  Temp := FData[Index1];
  FData[Index1] := FData[Index2];
  FData[Index2] := Temp;
end;

initialization
  Value := TSettings.Create;
  Setup;
finalization
  Value.Free;
end.

{
    Add(Part_PrivateSettings, 'Первый запуск',      'FirstStart',     rtBoolean, True);
    Add(Part_PrivateSettings, 'Обновление',         'UpdatedVersion', rtString,  '');
    Add(Part_PrivateSettings, 'Целевой СК',         'TargetBS',       rtFloat,   5.0);
    Add(Part_PrivateSettings, 'Наибольшая доза',    'MaxDose',        rtInteger, 10);
    Add(Part_PrivateSettings, 'Мин. СК до еды',     'BS1',            rtFloat,   3.7);
    Add(Part_PrivateSettings, 'Макс. СК до еды',    'BS2',            rtFloat,   6.2);
    Add(Part_PrivateSettings, 'Макс. СК после еды', 'BS3',            rtFloat,   7.8);
    Add(Part_PrivateSettings, 'Балансировать',      'Balance',        rtBoolean, True);
    Add(Part_PrivateSettings, 'Норма белков',       'NormProts',      rtInteger, 120);
    Add(Part_PrivateSettings, 'Норма жиров',        'NormFats',       rtInteger, 90);
    Add(Part_PrivateSettings, 'Норма углеводов',    'NormCarbs',      rtInteger, 350);

    Add(Part_AnalyzeSettings, 'Обрабатывать дней',  'DaysProcess',        rtInteger, 30);
    Add(Part_AnalyzeSettings, 'Адаптация',          'Adaptation',         rtFloat,   0.75);
    Add(Part_AnalyzeSettings, 'Режим обновления',   'UpdateKMode',        rtInteger, 1);
    Add(Part_AnalyzeSettings, 'Постпрандиал',       'PostPrandTime',      rtInteger, 210);
    Add(Part_AnalyzeSettings, 'Сокр.постпрандиал',  'ShortPostPrandTime', rtInteger, 20);
    Add(Part_AnalyzeSettings, 'Точки',              'ShowPoints',         rtBoolean, False);

    Add(Part_InterfaceSettings, 'PanelAddFood.Width',                  'DiaryFoodWidth',     rtInteger, 300);
    Add(Part_InterfaceSettings, 'PanelBaseFood.Width',                 'PanelBaseFood.Width',rtFloat,   0.5);
    Add(Part_InterfaceSettings, 'FontSize',                            'FontSize',           rtInteger, 10);
    Add(Part_InterfaceSettings, 'FontName',                            'FontName',           rtString,  'Arial');
    Add(Part_InterfaceSettings, 'ReservePath',                         'LastReservePath',    rtString,  'C:\Compensation Reserve\|');
    Add(Part_InterfaceSettings, 'ProcShow',                            'ProcShow',           rtInteger, 3);
    Add(Part_InterfaceSettings, 'DoubleBuffered',                      'DoubleBuffered',     rtBoolean, True);
    Add(Part_InterfaceSettings, 'ShadowEditors',                       'ShadowEditors',      rtBoolean, True);
    Add(Part_InterfaceSettings, 'AnimatePanels',                       'AnimatePanels',      rtBoolean, True);
    Add(Part_InterfaceSettings, 'Период наблюдения за использованием', 'AnUsingPeriod',      rtInteger, 30);
    Add(Part_InterfaceSettings, 'DishModFactor',                       'DishModFactor',      rtInteger, 4);
    Add(Part_InterfaceSettings, 'Оптимизировать базы',                 'OptimizeBases',      rtBoolean, True);
    Add(Part_InterfaceSettings, 'Сворачивать в трей',                  'MinToTray',          rtBoolean, False);
    Add(Part_InterfaceSettings, 'MealNotifies',                        'MealNotifies',       rtBoolean, True);
    Add(Part_InterfaceSettings, 'Цветные редакторы',                   'ColoredEditors',     rtBoolean, True);
    Add(Part_InterfaceSettings, 'Проверять обновления',                'CheckUpdates',       rtBoolean, True);
    Add(Part_InterfaceSettings, 'Последняя проверка',                  'LastUpdateCheck',    rtInteger, 0);
    Add(Part_InterfaceSettings, 'AutosaveInterval',                    'AutosaveInterval',   rtInteger, 5);
    Add(Part_InterfaceSettings, 'CarbsInfo',                           'CarbsInfo',          rtBoolean, True);

    Add(Part_Colors, 'StdBlood',   'Color_StdBlood',   rtInteger, $FFEEE6);
    Add(Part_Colors, 'StdIns',     'Color_StdIns',     rtInteger, $FFFFFF);
    Add(Part_Colors, 'StdMeal',    'Color_StdMeal',    rtInteger, $DDFFFF);
    Add(Part_Colors, 'StdNote',    'Color_StdNote',    rtInteger, $E4FFD8);
    Add(Part_Colors, 'StdBloodPP', 'Color_StdBloodPP', rtInteger, $F8E4F0);
    Add(Part_Colors, 'SelBlood',   'Color_SelBlood',   rtInteger, $F7DDCC);
    Add(Part_Colors, 'SelIns',     'Color_SelIns',     rtInteger, $F0F0F0);
    Add(Part_Colors, 'SelMeal',    'Color_SelMeal',    rtInteger, $99FFFF);
    Add(Part_Colors, 'SelNote',    'Color_SelNote',    rtInteger, $CAFFB3);
    Add(Part_Colors, 'SelBloodPP', 'Color_SelBloodPP', rtInteger, $F2CEE6); 

    Add(Part_BaseCols, 'FoodP', 'FoodP',  rtBoolean, True);
    Add(Part_BaseCols, 'FoodF', 'FoodF',  rtBoolean, True);
    Add(Part_BaseCols, 'FoodC', 'FoodC',  rtBoolean, True);
    Add(Part_BaseCols, 'FoodV', 'FoodV',  rtBoolean, True);
    Add(Part_BaseCols, 'FoodGI','FoodGI', rtBoolean, False);
    Add(Part_BaseCols, 'DishM', 'DishM',  rtBoolean, False);
    Add(Part_BaseCols, 'DishP', 'DishP',  rtBoolean, True);
    Add(Part_BaseCols, 'DishF', 'DishF',  rtBoolean, True);
    Add(Part_BaseCols, 'DishC', 'DishC',  rtBoolean, True);
    Add(Part_BaseCols, 'DishV', 'DishV',  rtBoolean, False);
    Add(Part_BaseCols, 'DishD', 'DishD',  rtBoolean, True);

    Add(Part_Export, 'Замеры СК',          'Export_AddBlood', rtBoolean, True);
    Add(Part_Export, 'Инъекции',           'Export_AddIns',   rtBoolean, True);
    Add(Part_Export, 'Приёмы пищи',        'Export_AddMeal',  rtBoolean, True);
    Add(Part_Export, 'Заметки',            'Export_AddNote',  rtBoolean, True);
    Add(Part_Export, 'Список блюд',        'Export_ExtMeal',  rtBoolean, False);
    Add(Part_Export, 'Режим приёмов пищи', 'Export_MealMode', rtInteger, 1);
    Add(Part_Export, 'Разрывы',            'Export_Breaks',   rtBoolean, True);

    Add(Part_Internet, 'Синхронизация', 'AutoSync',  rtBoolean, True);
    Add(Part_Internet, 'Логин',         'Login',     rtString,  '');
    Add(Part_Internet, 'Пароль',        'Password',  rtString,  '');
    Add(Part_Internet, 'Сервер',        'ServerURL', rtString,  'http://diacomp.16mb.com/');
    Add(Part_Internet, 'LastSync',      'LastSync',  rtString,  '0');

}
