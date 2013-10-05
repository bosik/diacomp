unit UnitReserve;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, DiaryInterface,
  ComCtrls, ImgList, pngimage, ActiveX,
  shlobj, shellapi, DiaryCore, SettingsINI;

type
  TSyncState = (
    stUnknow,
    stTest,
    stEqual,
    stFlashToComp,
    stFlashToCompProcess,
    stCompToFlash,
    stCompToFlashProcess,
    stDoubleSync,
    stDoubleSyncProcess,
    stWrongPath
  );

  TFormSync = class(TAutosetupForm)
    ButtonOK: TBitBtn;
    GroupSyncSource: TGroupBox;
    ComboPath: TComboBox;
    ButtonBrowse: TButton;
    Shape1: TShape;
    GroupOperations: TGroupBox;
    ImageState: TImage;
    ButtonTest: TSpeedButton;
    ButtonSync: TSpeedButton;
    LabelState: TLabel;
    procedure FormShow(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonReserveFlashClick(Sender: TObject);
    procedure ButtonTestClick(Sender: TObject);
    procedure ComboPathChange(Sender: TObject);
    procedure ButtonSyncClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonBrowseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  protected
    procedure Designer; override;
  private
    procedure Test;
    procedure SetState(NewState: TSyncState);
  public
    { Public declarations }
  end;

var
  FormSync: TFormSync;
  Tested: boolean;
  //FoodDir:  TSyncDir;
  //DishDir:  TSyncDir;
  //DiaryDir: TSyncDir;
  Path: string;

  FirstShow: boolean = true;
  States: array of TPicture;
  State: TSyncState;
const
  StateNames: array[TSyncState] of string = (
    'Не проверено',
    'Проверка...',
    'Данные синхронизированы',
    'Можно обновить данные',
    'Выполняется синхронизация...',
    'Можно резервировать данные',
    'Выполняется синхронизация...',
    'Можно обменяться данными',
    'Выполняется синхронизация...',
    'Указанный диск недоступен'
  );
  FOLDER_BASES = 'Bases\';

implementation

{$R *.dfm}

{========================================================}
procedure TFormSync.SetState(NewState: TSyncState);
{========================================================}
begin
  if Byte(NewState)<length(States) then
  begin
    ImageState.Picture.Assign(States[Byte(NewState)]);
    LabelState.Caption := StateNames[NewState];
  end else
  begin
    ImageState.Canvas.FillRect(ImageState.ClientRect);
    LabelState.Caption := '';
  end;
  State := NewState;
  Application.ProcessMessages;
end;

{========================================================}
procedure TFormSync.FormShow(Sender: TObject);
{========================================================}

 (* { FileName без WORK_FOLDER }
  procedure LoadPicture(Index: integer; const FileName: string);
  begin
    if FileExists(WORK_FOLDER+FileName) then
    begin
      States[Index] := TPicture.Create;
      States[Index].LoadFromFile(WORK_FOLDER+FileName);
    end;
  end;

var
  i: integer;
  buf: string; *)
begin
  (*PlaceCenter(FormSync);

  if FirstShow then
  begin
    // FirstShow := false; // см. ниже

    { загрузка путей }
    ComboPath.Clear;
    for i := 1 to length(Value['LastReservePath']) do
    if String(Value['LastReservePath'])[i]='|' then
    begin
      ComboPath.Items.Add(buf);
      buf := '';
    end else
      buf := buf + String(Value['LastReservePath'])[i];

    if ComboPath.Items.Count>0 then
      ComboPath.ItemIndex := 0;

    { загрузка картинок }
    SetLength(States,10);

    LoadPicture(0, IMAGE_SYNC_UNKNOW);
    LoadPicture(1, IMAGE_SYNC_TESTING);
    LoadPicture(2, IMAGE_SYNC_EQUAL);
    LoadPicture(3, IMAGE_SYNC_FLASH_COMP);
    LoadPicture(4, IMAGE_SYNC_FLASH_COMP_PROCESS);
    LoadPicture(5, IMAGE_SYNC_COMP_FLASH);
    LoadPicture(6, IMAGE_SYNC_COMP_FLASH_PROCESS);
    LoadPicture(7, IMAGE_SYNC_DOUBLE_SYNC);
    LoadPicture(8, IMAGE_SYNC_DOUBLE_SYNC_PROCESS);
    LoadPicture(9, IMAGE_SYNC_WRONG_PATH);
  end;

  SetState(stUnknow);

  if FirstShow then
  begin
    FirstShow := false;

    ImageState.Top := 2*DGroup + BORD;
    ButtonTest.Top := 2*DGroup + BORD;
    ButtonSync.Top := ButtonTest.Top + ButtonTest.Height + BORD;
    LabelState.Top := ButtonSync.Top + ButtonSync.Height + BORD;

    ImageState.Left := BORD;
    ButtonTest.Left := ImageState.Left + ImageState.Width + BORD;
    ButtonSync.Left := ImageState.Left + ImageState.Width + BORD;
    LabelState.Left := ImageState.Left + ImageState.Width + BORD;
    ButtonTest.Width := GroupOperations.Width - ButtonTest.Left - BORD;
    ButtonSync.Width := GroupOperations.Width - ButtonSync.Left - BORD;
    LabelState.Width := GroupOperations.Width - LabelState.Left - BORD;
    GroupOperations.Height := ImageState.Top + ImageState.Height + 2*BORD;
    ButtonOK.Top := GroupOperations.Top + GroupOperations.Height + BORD;
    ButtonOK.Left := ClientWidth - ButtonOK.Width;

    ClientHeight := ButtonOK.Top + ButtonOK.Height {+BORD in BorderWidth};
  end;     *)
end;

{========================================================}
procedure TFormSync.FormDestroy(Sender: TObject);
{========================================================}
var
  i: integer;
begin
  for i := 0 to high(States) do
    States[i].Free;
  SetLength(States,0);
end;

procedure TFormSync.ButtonOKClick(Sender: TObject);
begin
  { saving settings }
  {TryStrToInt(EditID.Text, Value_ID);
  Value_Password := EditPassword.Text;}

  Close;
end;

{==============================================================================}
procedure TFormSync.ButtonReserveFlashClick(Sender: TObject);
{==============================================================================}
{var
  Path: string;}
begin
 (* Path := ComboPath.Text;
  if Path = '' then
  begin
    ErrorMessage('Введите путь');
    ComboPath.SetFocus;
    exit;
  end;

  if Path[length(Path)]<>'\' then
  begin
    Path := Path+'\';
    ComboPath.Text := ComboPath.Text+'\';
  end;

  { CREATING FOLDERS [1] }
  if not DirectoryExists(Path) then
  begin
    if not CreateDirectory(PChar(Path),nil) then
    begin
      ErrorMessage('Невозможно создать указанную папку');
      ComboPath.SetFocus;
      exit;
    end;
  end;

  { CREATING FOLDERS [1] }
  CreateDirectory(PChar(Path+FOLDER_BASES),nil);

  { COPYING DIARY [1] }
  CopyFile(
    PChar(WORK_FOLDER+Diary_FileName),
    PChar(Path+Diary_FileName),
    false
  );
  (*
  if CheckBoxBases.Checked then
  begin
    { COPYING BASES [1] }
    CopyFile(
      PChar(WORK_FOLDER+FoodBase_FileName),
      PChar(Path+FoodBase_FileName),
      false
    );
    { COPYING BASES [1] }
    CopyFile(
      PChar(WORK_FOLDER+DishBase_FileName),
      PChar(Path+DishBase_FileName),
      false
    );
  end;

  if CheckBoxProgram.Checked then
  begin
    { COPYING PROGRAM [1] }
    CopyFile(
      PChar(Application.EXEName),
      PChar(Path+ExtractFileName(Application.EXEName)),
      false
    );

    { COPYING ANALYZE [1] }
    CopyFile(
      PChar(WORK_FOLDER+ANALYZE_LIB_FileName),
      PChar(Path+ANALYZE_LIB_FileName),
      false
    );

    { COPYING SETTINGS [0] }
    CopyFile(
      PChar(WORK_FOLDER+SettingsFileName),
      PChar(Path+SettingsFileName),
      false
    );
  end;

  MessageDlg(MESSAGE_SUCCESSFULLY_RESERVED,mtInformation,[mbOK],0);    *)
end;

procedure TFormSync.ButtonTestClick(Sender: TObject);
begin
  Test;
end;

procedure TFormSync.Test;
begin
  (*{ 1. Проверка пути }
  Path := ComboPath.Text;
  if Path = '' then
  begin
    SetState(stWrongPath);
    ErrorMessage('Введите путь');
    ComboPath.SetFocus; 
    Exit;
  end;

  if Path[length(Path)]<>'\' then
  begin
    Path := Path+'\';
    ComboPath.Text := ComboPath.Text+'\';
  end;

  if (Path[1]<>'\')and
     (not DirectoryExists(Copy(Path,1,3))) then
  begin
    SetState(stWrongPath);
    ErrorMessage(StateNames[stWrongPath]);
    ComboPath.SetFocus;
    Exit;
  end;

  ButtonTest.Enabled := false;

  SetState(stTest);
  sleep(200);

  { 2. Проверка файлов }
  FoodDir := GetSyncDirection(
    WORK_FOLDER+FoodBase_FileName,
    Path+FoodBase_Name);
  DishDir := GetSyncDirection(
    WORK_FOLDER+DishBase_FileName,
    Path+DishBase_Name);
  DiaryDir := GetSyncDirection(
    WORK_FOLDER+Diary_FileName,
    Path+Diary_Name);

  if (FoodDir = sdEqual) and
     (DishDir = sdEqual) and
     (DiaryDir = sdEqual)
  then SetState(stEqual) else

  if ((FoodDir=sdBack)or(DishDir=sdBack)or(DiaryDir=sdBack))and
     ((FoodDir=sdForward)or(DishDir=sdForward)or(DiaryDir=sdForward))
  then  SetState(stDoubleSync) else

  if (FoodDir=sdBack)or(DishDir=sdBack)or(DiaryDir=sdBack)
  then  SetState(stFlashToComp) else

  if (FoodDir=sdForward)or(DishDir=sdForward)or(DiaryDir=sdForward)
  then  SetState(stCompToFlash);

  ButtonTest.Enabled := true;  *)
end;

procedure TFormSync.ComboPathChange(Sender: TObject);
begin
  SetState(stUnknow);
end;

procedure TFormSync.ButtonSyncClick(Sender: TObject);
{var
  Ind: integer;  }
begin
  (*ButtonSync.Enabled := false;
  Test;

  if State = stEqual then
    InfoMessage('Данные уже синхронизированы') else
  begin
    case State of
      stFlashToComp: SetState(stFlashToCompProcess);
      stCompToFlash: SetState(stCompToFlashProcess);
      stDoubleSync: SetState(stDoubleSyncProcess);
    end;

    if not DirectoryExists(Path) then
    begin
      if not CreateDirectory(PChar(Path),nil) then
      begin
        SetState(stWrongPath);
        ErrorMessage('Невозможно создать указанную папку');
        ComboPath.SetFocus;
        ButtonSync.Enabled := true;
        EXIT;
      end;
    end;

    CreateDirectory(PChar(Path),nil);

    case FoodDir of
      sdBack: begin
                CopyFile(
                    PChar(Path+FoodBase_Name),
                    PChar(WORK_FOLDER+FoodBase_FileName),
                    false
                   );
                Form1.LoadFoodBase;
                ShowFoodBase(Form1.ListFood,true);
                Form1.UpdateFoodCombos;
              end;
      sdForward: CopyFile(
                    PChar(WORK_FOLDER+FoodBase_FileName),
                    PChar(Path+FoodBase_Name),
                    false
                   );
    end;

    case DishDir of
      sdBack: begin
                CopyFile(
                    PChar(Path+DishBase_Name),
                    PChar(WORK_FOLDER+DishBase_FileName),
                    false
                   );
                Form1.LoadDishBase;
                ShowDishBase(Form1.ListDish,true);
                Form1.UpdateDishCombos;
              end;

      sdForward: CopyFile(
                    PChar(WORK_FOLDER+DishBase_FileName),
                    PChar(Path+DishBase_Name),
                    false
                   );
    end;

    case DiaryDir of
      sdBack: begin
                CopyFile(
                    PChar(Path+Diary_Name),
                    PChar(WORK_FOLDER+Diary_FileName),
                    false
                   );
                Form1.LoadDiary;
                Form1.Diary1.OpenPage(Trunc(Form1.CalendarDiary.Date),true);
              end;
      sdForward: CopyFile(
                    PChar(WORK_FOLDER+Diary_FileName),
                    PChar(Path+Diary_Name),
                    false
                   );
    end;

    Test;

    if State = stEqual then
    begin
      Ind := ComboPath.Items.IndexOf(Path);
      if Ind = -1 then
        ComboPath.Items.Insert(0,Path)
      else
      begin
        ComboPath.Items.Move(Ind,0);
        ComboPath.ItemIndex := 0;
      end;
    end else
      ErrorMessage(MESSAGE_SYNC_PROBLEM);
  end;

  ButtonSync.Enabled := true;  *)
  InfoMessage('Синхронизация отключена за неуплату');
end;

{=======================================================}
function SelectDirectory(const Caption: string; const Root: WideString;
  out Directory: WideString): Boolean;
{=======================================================}
var
  WindowList: Pointer;
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
  ActiveWindow: HWND;
begin
  Result := False;
  Directory := '';
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      if Root <> '' then
      begin
        SHGetDesktopFolder(IDesktopFolder);
        IDesktopFolder.ParseDisplayName(0, nil,
          POleStr(Root), Eaten, RootItemIDList, Flags);
      end;
      with BrowseInfo do
      begin
        hwndOwner := 0;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS;
      end;
      ActiveWindow := GetActiveWindow;
      WindowList := DisableTaskWindows(0);
      try
        ItemIDList := ShBrowseForFolder(BrowseInfo);
      finally
        EnableTaskWindows(WindowList);
        SetActiveWindow(ActiveWindow);
      end;
      Result :=  ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

{==============================================================================}
procedure TFormSync.ButtonBrowseClick(Sender: TObject);
{==============================================================================}
var
  Dir: WideString;
begin
  Dir := ComboPath.Text;
  if SelectDirectory('Выберите каталог', '', Dir) then
    ComboPath.Text := Dir+'\'+FOLDER_BASES;
end;

{==============================================================================}
procedure TFormSync.FormClose(Sender: TObject; var Action: TCloseAction);
{==============================================================================}
var
  i: integer;
begin
  Value['LastReservePath'] := '';
  for i := 0 to ComboPath.Items.Count-1 do
  begin
    Value['LastReservePath'] :=
      Value['LastReservePath'] +
      ComboPath.Items[i]+'|';
  end;
end;

procedure TFormSync.Designer;
begin
  BorderWidth := BORD;
  Shape1.Height := BORD;
end;

end.
