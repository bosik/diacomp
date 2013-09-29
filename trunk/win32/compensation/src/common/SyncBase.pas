unit SyncBase;

interface

(*uses
  DiaryDatabase, SysUtils;

type
  TBaseType = (btUnknown, btDiary, btFoodBase, btDishBase);
  TSyncDir  = (sdCannot, sdBack, sdForward, sdEqual);

  { Результат - можно ли синхронизировать с данным файлом }
  { Для этого:                                            }
  {   - файл должен существовать;                         }
  {   - у него должен быть стандартный заголовок.         }
  function GetSyncInfo(const FileName: string; var Stamp: TDateTime;
    var BaseType: TBaseType): boolean;

  function GetSyncDirection(const File1,File2: string): TSyncDir;  *)

implementation

(*

{========================================================}
function GetSyncInfo(const FileName: string; var Stamp: TDateTime;
  var BaseType: TBaseType): boolean;
{========================================================}
var
  f: TextFile;
  str: string;
begin
  if not FileExists(FileName) then
  begin
    result:=false;
    BaseType:=btUnknown;
    exit;
  end;

  try
    AssignFile(f,FileName);
    Reset(f);
    Readln(f,str);

    try
      if (str = DIARY_FORMAT) then
      begin
        BaseType:=btDiary;
        Readln(f,str);
        Stamp:=StrToDateTime(str);
        result:=true;
      end else
      if (str = FOODBASE_FORMAT) then
      begin
        BaseType:=btFoodBase;
        Readln(f,str);
        Stamp:=StrToDateTime(str);
        result:=true;
      end else
      if (str = DISHBASE_FORMAT) then
      begin
        BaseType:=btDishBase;
        Readln(f,str);
        Stamp:=StrToDateTime(str);
        result:=true;
      end else
      begin
        BaseType:=btUnknown;
        result:=false;
      end;
    finally
      CloseFile(f);
    end;
  except
    result:=false;
    BaseType:=btUnknown;
  end;
end;

{========================================================}
function GetSyncDirection(const File1,File2: string): TSyncDir;
{========================================================}
var
  Stamp1, Stamp2: TDateTime;
  Type1, Type2: TBaseType;
begin
  GetSyncInfo(File1, Stamp1, Type1);
  GetSyncInfo(File2, Stamp2, Type2);

  if ((Type1 = btUnknown)and(Type2 = btUnknown)) or
     ((Type1 <> btUnknown)and(Type2 <> btUnknown)and(Type1 <> Type2) )
  then
    result:=sdCannot else
  begin
    if (Type1 = btUnknown) then
      result:=sdBack else
    if (Type2 = btUnknown) then
      result:=sdForward else

    if Stamp1 < Stamp2 then
      result:=sdBack else
    if Stamp1 > Stamp2 then
      result:=sdForward
    else
      result:=sdEqual;
  end;
end;   }
*)

end.
