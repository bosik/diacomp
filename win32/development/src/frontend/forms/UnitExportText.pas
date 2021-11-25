unit UnitExportText;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  DiaryPage, DiaryRecords, DiaryRoutines, DiaryInterface, DiaryCore, BusinessObjects;

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

uses MainUnit, SettingsINI, DiaryDAO;

{$R *.dfm}

{ TFormExportText }

{=========================================================}
procedure TFormExportText.CreateLog(Sender: TObject);
{=========================================================}
type
  TMealType = (mtBU, mtCarbs, mtCarbsProts);
const
  BREAK_TIME = 3 / HourPerday;
  BREAK_SYMB = ' - ';
var
  IncBreaks: boolean;
  LastEventTime: TDateTime;
  IncBS: boolean;
  IncIns: boolean;
  IncMeal: boolean;
  IncCont: boolean;
  IncNote: boolean;

  procedure CheckBreak(Rec: TCustomRecord);
  begin
    if IncBreaks then
    begin
      if (LastEventTime > 0) and (Rec.Time - LastEventTime >= BREAK_TIME) then
        MemoOut.Lines.Add('');
      LastEventTime := Rec.Time;
    end;
  end;

  function CheckedCount(Recs: TVersionedList): integer;
  var
    i: integer;
    Rec: TCustomRecord;
  begin
    Result := 0;
    for i := 0 to High(Recs) do
    begin
      Rec := TCustomRecord(Recs[i].Data);

      if (Rec.RecType = TBloodRecord) and IncBS   then inc(Result) else
      if (Rec.RecType = TInsRecord)   and IncIns  then inc(Result) else
      if (Rec.RecType = TMealRecord)  and IncMeal then inc(Result) else
      if (Rec.RecType = TNoteRecord)  and IncNote then inc(Result);
    end;
  end;

  procedure WriteBlood(Output: TStrings; Rec: TVersioned);
  var
    Item: TBloodRecord;
  begin
    Item := TBloodRecord(Rec.Data);
    CheckBreak(Item);
    Output.Add(FormatDateTime(UTCToLocal(Item.Time)) + BREAK_SYMB + RealToStrZero(Item.Value) + ' ммоль/л');
  end;

  procedure WriteIns(Output: TStrings; Rec: TVersioned);
  var
    Item: TInsRecord;
  begin
    Item := TInsRecord(Rec.Data);
    CheckBreak(Item);
    Output.Add(FormatDateTime(UTCToLocal(Item.Time)) + BREAK_SYMB + '['+ RealToStr(Item.Value) + ' ЕД]');
  end;

  procedure WriteMeal(Output: TStrings; Rec: TVersioned; Mode: TMealType; IncludeContent: boolean);
  var
    Item: TMealRecord;
    k: integer;
  begin
    Item := TMealRecord(Rec.Data);
    CheckBreak(Item);

    case Mode of
      mtBU:         Output.Add(Format('%s' + BREAK_SYMB + '%s ХЕ', [
                      FormatDateTime(UTCToLocal(Item.Time)),
                      RealToStr(Item.Carbs / 12)
                    ]));
      mtCarbs:      Output.Add(Format('%s' + BREAK_SYMB + '%.0f г угл.', [
                      FormatDateTime(UTCToLocal(Item.Time)),
                      Item.Carbs
                    ]));
      mtCarbsProts: Output.Add(Format('%s' + BREAK_SYMB + '%.0f г угл., %.0f г белков', [
                      FormatDateTime(UTCToLocal(Item.Time)),
                      Item.Carbs,
                      Item.Prots
                    ]));
    end;

    if IncludeContent then
    begin
      for k := 0 to Item.Count - 1 do
        Output.Add('   ' + Item.Food[k].Name + ' (' + RealToStr(Item.Food[k].Mass)+')');
    end;
  end;

  procedure WriteNote(Output: TStrings; Rec: TVersioned);
  var
    Item: TNoteRecord;
  begin
    Item := TNoteRecord(Rec.Data);
    CheckBreak(Item);
    Output.Add(FormatDateTime(UTCToLocal(Item.Time)) + BREAK_SYMB + Item.Text);
  end;

var
  MealInfo: TMealType;
  Recs: TVersionedList;
  Rec: TCustomRecord;
  j, k: integer;
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
    MealInfo := mtCarbsProts
  else
    MealInfo := mtCarbs;

  MemoOut.Clear;

  if Picker1.Date > Picker2.Date then
  begin
    Exit;
  end;

  Recs := LocalSource.FindPeriod(Trunc(Picker1.Date), Trunc(Picker2.Date) + 1);

  try
    LastEventTime := 0;
    if (CheckedCount(Recs) > 0) then
    for j := Low(Recs) to High(Recs) do
    begin
      with MemoOut.Lines, Form1.DiaryView do
      begin
        //if Date > Picker1.Date then Add('');
        MemoOut.SelAttributes.Style := [fsBold];
  //      Add('====== ' + DateToStr(Date) + ' ======');
        MemoOut.SelAttributes.Style := [];

        Rec := TCustomRecord(Recs[j].Data);

        if (Rec.RecType = TBloodRecord) then
        begin
          if (IncBS) then
            WriteBlood(MemoOut.Lines, Recs[j]);
        end else

        if (Rec.RecType = TInsRecord) then
        begin
          if (IncIns) then
            WriteIns(MemoOut.Lines, Recs[j]);
        end else

        if (Rec.RecType = TMealRecord) then
        begin
          if (IncMeal) then
            WriteMeal(MemoOut.Lines, Recs[j], MealInfo, IncCont);
        end else

        if (Rec.RecType = TNoteRecord) then
        begin
          if (IncNote) then
            WriteNote(MemoOut.Lines, Recs[j]);
        end; 
      end;
    end;
  finally
    FreeRecords(Recs);
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
