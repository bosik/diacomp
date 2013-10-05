unit UnitExportText;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  DiaryDatabase, BusinessObjects, DiaryRoutines, DiaryInterface, DiaryCore;

type
  TFormExportText = class(TAutosetupForm)
    GroupPeriod: TGroupBox;
    Picker1: TDateTimePicker;
    Picker2: TDateTimePicker;
    Label1: TLabel;
    Label2: TLabel;
    Shape1: TShape;
    PanelButtons: TPanel;
    ButtonSave: TSpeedButton;
    ButtonClose: TBitBtn;
    MemoOut: TRichEdit;
    SaveDialog1: TSaveDialog;
    Shape3: TShape;
    PanelSettings: TPanel;
    GroupContent: TGroupBox;
    CheckAddBS: TCheckBox;
    CheckAddIns: TCheckBox;
    CheckAddMeal: TCheckBox;
    RadioMealCarbs: TRadioButton;
    RadioMealCarbsProts: TRadioButton;
    RadioMealBU: TRadioButton;
    CheckMealContent: TCheckBox;
    GroupFormat: TGroupBox;
    Splitter1: TSplitter;
    CheckBreaks: TCheckBox;
    Shape2: TShape;
    CheckAddNotes: TCheckBox;
    procedure CheckAddMealClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CreateLog(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  protected
    procedure Designer; override;
  public

  end;

var
  FormExportText: TFormExportText;

implementation

uses MainUnit, SettingsINI;

{$R *.dfm}

{ TFormExportText }

{=========================================================}
procedure TFormExportText.CreateLog(Sender: TObject);
{=========================================================}
type
  TMealType = (mtBU,mtCarbs,mtCarbsProts);
const
  BREAK_TIME = 3*60;
  BREAK_SYMB = ' - ';
var
  IncBreaks: boolean;
  LastEventTime: integer;

  procedure CheckBreak(Page: TDiaryPage; Index: integer);
  begin
    if IncBreaks then
    begin
      if Page[Index].Time-LastEventTime >= BREAK_TIME then
        MemoOut.Lines.Add('');
      LastEventTime := Page[Index].Time;
    end;
  end;

var
  IncBS: boolean;
  IncIns: boolean;
  IncMeal: boolean;
  IncCont: boolean;
  IncNote: boolean;
  MealInfo: TMealType;

  j,k: integer;
  //TempDate: TDate;
  Date: integer;

  function CheckedCount(Page: TDiaryPage): integer;
  var
    i: integer;
  begin
    Result := 0;
    for i := 0 to Page.Count - 1 do
    begin
      if (Page[i].RecType = TBloodRecord) and IncBS   then inc(Result) else
      if (Page[i].RecType = TInsRecord)   and IncIns  then inc(Result) else
      if (Page[i].RecType = TMealRecord)  and IncMeal then inc(Result) else
      if (Page[i].RecType = TNoteRecord)  and IncNote then inc(Result);
    end;
  end;
  
begin
  // при настройке интерфейса трогаются галки этого окна
  if not visible then Exit;

  { инициализация }
  IncBS := CheckAddBS.Checked;
  IncIns := CheckAddIns.Checked;
  IncMeal := CheckAddMeal.Checked;
  IncCont := CheckMealContent.Checked;
  IncNote := CheckAddNotes.Checked;
  IncBreaks := CheckBreaks.Checked;

  if RadioMealBU.Checked then
    MealInfo := mtBU else
  if RadioMealCarbs.Checked then
    MealInfo := mtCarbs else
  if RadioMealCarbsProts.Checked then
    MealInfo := mtCarbsProts else
    MealInfo := mtCarbs;

  MemoOut.Clear;

  if Picker1.Date > Picker2.Date then
  begin
    {TempDate := Picker1.Date;
    Picker1.Date := Picker2.Date;
    Picker2.Date := TempDate; }
    exit;
  end;

  //n1 := Form1.DiaryBase.FindPage(Picker1.Date);
  //n2 := Form1.DiaryBase.FindPage(Picker2.Date);

  { на всякий случай оставим }
  {if (n1>n2)and(n1<>-1)and(n2<>-1) then
  begin
    n1 := n1 xor n2;
    n2 := n1 xor n2;
    n1 := n1 xor n2;
  end; }

  { заполнение }
  {if (n1=-1) and (n2>-1) then
    n1 := 0 else
  if (n1<>-1) and (n2=-1) then
    n2 := Form1.DiaryBase.Count-1 else
  if (n1=-1) and (n2=-1) then
    exit;    }

  for Date := Trunc(Picker1.Date) to Trunc(Picker2.Date) do
  begin    

    if CheckedCount(Diary[Date]) > 0 then
    with MemoOut.Lines, Form1.DiaryView do
    begin
      if Date > Picker1.Date then Add('');
      MemoOut.SelAttributes.Style := [fsBold];
      Add('====== '+DateToStr(Diary[Date].Date)+' ======');
      MemoOut.SelAttributes.Style := [];
      LastEventTime := 1440;

      for j := 0 to Diary[Date].Count - 1 do
      begin
        if (Diary[Date][j].RecType = TBloodRecord) and (IncBS) then
        begin
          CheckBreak(Diary[Date], j);
          Add(TimeToStr(Diary[Date][j].Time)+BREAK_SYMB+RealToStrZero(TBloodRecord(Diary[Date][j]).Value)
          +' ммоль/л');
        end  else

        if (Diary[Date][j].RecType = TInsRecord) and (IncIns) then
        begin
          CheckBreak(Diary[Date], j);
          Add(TimeToStr(Diary[Date][j].Time)+BREAK_SYMB+'['+RealToStr(TInsRecord(Diary[Date][j]).Value)+' ЕД]');
        end else

        if (Diary[Date][j].RecType = TMealRecord) and (IncMeal) then
        begin
          CheckBreak(Diary[Date], j);
          case MealInfo of
            mtBU:         Add(TimeToStr(Diary[Date][j].Time)+BREAK_SYMB+
                            RealToStr(TMealRecord(Diary[Date][j]).Carbs/12)+' ХЕ');
            mtCarbs:      Add(TimeToStr(Diary[Date][j].Time)+BREAK_SYMB+
                            IntToStr(Round(TMealRecord(Diary[Date][j]).Carbs))+'г угл.');
            mtCarbsProts: Add(TimeToStr(Diary[Date][j].Time)+BREAK_SYMB+
                            IntToStr(Round(TMealRecord(Diary[Date][j]).Carbs))+'г угл., '+
                            IntToStr(Round(TMealRecord(Diary[Date][j]).Prots))+'г белков');
          end;
          if IncCont then
          for k := 0 to TMealRecord(Diary[Date][j]).Count-1 do
            Add('   '+TMealRecord(Diary[Date][j]).Food[k].Name+' ('+
            RealToStr(TMealRecord(Diary[Date][j]).Food[k].Mass)+')');
        end else

        if (Diary[Date][j].RecType = TNoteRecord) and (IncNote) then
        begin
          CheckBreak(Diary[Date],j);
          Add(TimeToStr(Diary[Date][j].Time)+BREAK_SYMB+TNoteRecord(Diary[Date][j]).Text);
        end; 
      end;
    end;
  end;
end;

procedure TFormExportText.CheckAddMealClick(Sender: TObject);
begin
  CheckMealContent.Enabled := CheckAddMeal.Checked;
  RadioMealCarbs.Enabled := CheckAddMeal.Checked;
  RadioMealCarbsProts.Enabled := CheckAddMeal.Checked;
  RadioMealBU.Enabled := CheckAddMeal.Checked;
  CreateLog(nil);
end;

procedure TFormExportText.ButtonSaveClick(Sender: TObject);
var
  FN: string;
  i: integer;
begin
  if SaveDialog1.Execute then
  begin
    FN := SaveDialog1.FileName;
    case SaveDialog1.FilterIndex of
      1:  begin
            if pos('.TXT',UpperCase(FN))<>length(FN)-3 then
              FN := FN+'.txt';
            with TStringList.Create do
            begin
              for i := 0 to MemoOut.Lines.Count-1 do
                Add(MemoOut.Lines[i]);
              SaveToFile(FN);
              Free;
            end;
          end;

      2:  begin
            if pos('.RTF',UpperCase(FN))<>length(FN)-3 then
              FN := FN+'.rtf';
            MemoOut.Lines.SaveToFile(FN);
          end;
    end;
  end;
end;

procedure TFormExportText.FormShow(Sender: TObject);
begin
  PlaceCenter(FormExportText);
  CreateLog(nil);
end;

procedure TFormExportText.FormResize(Sender: TObject);
begin
  GroupFormat.Width := 4*((Width-16) div 2) div 5;
end;

procedure TFormExportText.FormCreate(Sender: TObject);
begin
  FormExportText.Picker1.Date := now;
  FormExportText.Picker2.Date := now;
end;

procedure TFormExportText.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Value['Export_AddBlood'] := CheckAddBS.Checked;
  Value['Export_AddIns']   := CheckAddIns.Checked;
  Value['Export_AddMeal']  := CheckAddMeal.Checked;
  Value['Export_AddNote']  := CheckAddNotes.Checked;
  Value['Export_ExtMeal']  := CheckMealContent.Checked;
  Value['Export_Breaks']   := CheckBreaks.Checked;

  if RadioMealCarbs.Checked then Value['Export_MealMode'] := 1 else
  if RadioMealCarbsProts.Checked then Value['Export_MealMode'] := 2 else
  if RadioMealBU.Checked then Value['Export_MealMode'] := 3 else
    Value['Export_MealMode'] := 1;
end;

procedure TFormExportText.Designer;
begin
  CheckAddBS.Checked := Value['Export_AddBlood'];
  CheckAddIns.Checked := Value['Export_AddIns'];
  CheckAddMeal.Checked := Value['Export_AddMeal'];
  CheckAddNotes.Checked := Value['Export_AddNote'];
  CheckMealContent.Checked := Value['Export_ExtMeal'];
  CheckBreaks.Checked := Value['Export_Breaks'];

  case Value['Export_MealMode'] of
    1: RadioMealCarbs.Checked := true;
    2: RadioMealCarbsProts.Checked := true;
    3: RadioMealBU.Checked := true;
    else RadioMealCarbs.Checked := true;
  end;

  Picker1.Width := GroupPeriod.Width - Picker1.Left - 2*BORD;
  Picker2.Width := GroupPeriod.Width - Picker2.Left - 2*BORD;
  ButtonSave.Left := 0;
  ButtonClose.Left := PanelButtons.Width - ButtonClose.Width;
end;

end.
