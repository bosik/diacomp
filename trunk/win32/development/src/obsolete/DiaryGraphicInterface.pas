unit DiaryGraphicInterface;

{ Модуль для графической части интерфейса дневника }

interface

uses
  Graphics, {colors, etc.}
  Classes, {Rect()}
  Windows, {TRect}
  SysUtils, {IntToStr}
  ExtCtrls, {TImage}
  Controls, {TMouseButton}
  DiaryDatabase,
  Dialogs, {ShowMessage}
  Menus, {PopupMenu}
  DiaryInterface, {tools}
  AnalyzeUnit {SearchInjectedDose};

type
  TPanelRect = record
    cNumb: integer;
    Rect: TRect;
    TimeRect: TRect;
    Recs: array of TRect;
  end;

  TPlace = (cpNoWhere,cpPanel,cpTime,cpRec);

  TClickInfo = record
    PlaceType: TPlace;
    RecordType: TRecType;
    PanelNumber,RecNumber,LineNumber: integer;
  end;

  { private }
  //function AddPanelRect(r: TRect; Tag: integer): integer;

  { public }
  {procedure DrawDiaryPage(Image: TImage; const Page: TDiaryPage);
  procedure EraseDiaryPage(Image: TImage);

  procedure GetMousePlace(x,y: integer; const Page: TDiaryPage;
    var ClickInfo: TClickInfo);
  procedure ProcessClick(x,y: integer; Button: TMouseButton; Image: TImage;
    var Page: TDiaryPage; const Diary: TDiary;
    PopupDish,PopupPanel: TPopupMenu);
  procedure ProcessMove(x,y: integer; Image: TImage; const Page: TDiaryPage);
          }
  procedure ChangeTime(var Page: TDiaryPage; const Diary: TDiary;
    RecType: TRecType; N: integer; BackImage: TImage);

//var
  //PanelRects: array of TPanelRect;
  //LastClickedTime: cardinal = 0;
  //LastX,LastY: integer;
  //SelectedPanel: integer = -1;
  //SelectedLine: integer  = -1;
//const
  //DoubleClickTime  = 350;
  //DoubleClickShift = 20;
   (*
  { цвета }
  BACKGROUND       = clWhite;
  PANEL_BLOOD      = $F8E4D8;
  PANEL_INS        = $FFFFFF;
  PANEL_MEAL       = $E1FFFF;
  PANEL_COLORS: array[TRecType] of TColor = (PANEL_BLOOD,PANEL_INS,PANEL_MEAL);
  PANEL_LIGHT_BORD = clWhite;
  PANEL_DARK_BORD  = clGray;
  PANEL_SELECTED   = $A1FFFF;
  SELECTED_LINE    = clYellow;
  LINES_COLOR      = $FBF1EB;//$F8E4D8;

  FONT_SIZE = 8;   *)
implementation

{=========================================================}
procedure ErrorMessage(const Text: string);
{=========================================================}
begin
  MessageDlg(Text,mtError,[mbOK],0);
end;

(*
{=========================================================}
function AddPanelRect(r: TRect; Tag: integer): integer;
{=========================================================}
begin
  SetLength(PanelRects,length(PanelRects)+1);
  with PanelRects[high(PanelRects)] do
  begin
    Rect:=r;
    cNumb:=Tag;
  end;
  result:=high(PanelRects);
end;

{=========================================================}
procedure DrawDiaryPage(Image: TImage; const Page: TDiaryPage);
{=========================================================}
const
  Border       = 10;
  RightBorder  = 10;
  TimeLeftBord = 7;
  TimeTopBord  = 4;
var
  w,h: integer;
  RecsLeftBord: integer;
  i,k: integer;
  CurTop: integer;
  StandartTH: integer;
  buf: Graphics.TBitMap;

  procedure AddText(x,y: integer; const text: string; var R: TRect;
    Selected: boolean);
  { рисует текст, изменяет Rect }
  { Brush, Font должны быть настроены }
  begin
    R:=Rect(x,y,
      x+Buf.Canvas.TextWidth(text),
      y+Buf.Canvas.TextHeight(text)
    );
    if Selected then
      Buf.Canvas.Rectangle(
        x-1,y-1,
        x+Buf.Canvas.TextWidth(text)+2,
        y+Buf.Canvas.TextHeight(text)+2);
    Buf.Canvas.TextOut(x,y,text);
  end;

  procedure DrawPanel(r: TRect; Color: TColor; Selected: boolean);
  { рисует красивую панельку }
  begin
    with Buf.Canvas do
    begin
      Pen.Color:=PANEL_LIGHT_BORD;
      if Selected then
        Brush.Color:=PANEL_SELECTED
      else
        Brush.Color:=Color;
      Pen.Width:=1;

      Brush.Style:=bsSolid;
      Rectangle(r);
      Pen.Color:=PANEL_DARK_BORD;
      MoveTo(r.Left+1,r.Bottom-2);
      LineTo(r.Left+1,r.Top+1);
      LineTo(r.Right-1,r.Top+1);
      MoveTo(r.Left,r.Bottom);
      LineTo(r.Right,r.Bottom);
      LineTo(r.Right,r.Top-1);
    end;
  end;

  procedure AddPanel(ChroneNumber: integer;
    PanelColor: TColor; Time: integer;
    Recs: array of string;
    RecsFontStyle: TFontStyles);
  { рисует панель со временем, добавляет прямоугольник }
  var
    i,n,cnt: integer;
    r: TRect;
  begin
    with Buf.Canvas do
    begin
      cnt:=length(recs);
      if cnt=0 then cnt:=1;
      r:=Rect(
        Border,
        CurTop,
        w-RightBorder,
        CurTop+2*TimeTopBord+StandartTH*cnt);

      DrawPanel(r,PanelColor,SelectedPanel=ChroneNumber);

      n:=AddPanelRect(r,ChroneNumber);

      if SelectedPanel=n then
        Brush.Color:=PANEL_SELECTED
      else
        Brush.Color:=PanelColor;
      Font.Color:=clBlack;  {magic!}
      Font.Style:=[fsBold,fsUnderline]; {magic!}
      AddText(
        r.Left+TimeLeftBord,
        r.Top+TimeTopBord,
        TimeToStr(time),
        PanelRects[n].TimeRect,
        false
      );

      Font.Style:=RecsFontStyle;
      SetLength(PanelRects[n].Recs,length(Recs));

      if (SelectedPanel=n) then
      for i:=0 to high(recs) do
      begin
        if SelectedLine=i then // не слишком быстро, но лаконично
          Brush.Color:=SELECTED_LINE
        else
          Brush.Color:=PANEL_SELECTED;
        AddText(
          r.Left+RecsLeftBord,
          r.Top+TimeTopBord+StandartTH*i,
          recs[i],
          PanelRects[n].Recs[i],
          SelectedLine=i
        );
      end else
      for i:=0 to high(recs) do
      AddText(
        r.Left+RecsLeftBord,
        r.Top+TimeTopBord+StandartTH*i,
        recs[i],
        PanelRects[n].Recs[i],
        false
      );
    end;
    CurTop:=CurTop+(r.Bottom-r.Top)-1;
  end;

  function CreateRecsArray(const Meal: TMealRecord): TStringArray;
  var
    i: integer;
    _K,_Q: real;
  begin
    GetKoofs(Meal.Time,_k,_q);

    SetLength(result,length(Meal.Dish));
    for i:=0 to high(result) do
      result[i]:=Meal.Dish[i].Name+' ('+
      RealToStr(Meal.Dish[i].Mass)+') [+'+
      RealToStr(
        Meal.Dish[i].Carbs*_k
        )+']';;
  end;

  function F(n: integer): integer;
  begin
    if n=0 then result:=1 else result:=n;
  end;

begin
  try
    { создание буфера }
    buf:=Graphics.TBitMap.Create;
    { определение необходимого места }
    Buf.Canvas.Font.Size:=FONT_SIZE;
    Buf.Canvas.Font.Style:=[];
    StandartTH:=Buf.Canvas.TextHeight('0123456789')+2;
    h:=2*Border;
    for i:=0 to high(Page.Chron) do
    begin
      k:=Page.Chron[i].RecNumber;
      case Page.Chron[i].RecType of
        rtBlood,rtIns: h:=h+2*TimeTopBord+StandartTH;
        rtMeal: h:=h+2*TimeTopBord+StandartTH*F(length(Page.Meal[k].Dish));
      end;
    end;

    if h<Image.Tag then h:=Image.Tag;

    Image.Height:=h;
    Image.Picture.Bitmap.Height:=h;
    Image.Picture.Bitmap.Width:=Image.Width;
    Buf.Width:=Image.Width;
    Buf.Height:=h;

    { очистка регионов }
    SetLength(PanelRects,0);

    with Buf.Canvas do
    begin
      { очистка экрана - не нужна, вообще говоря }
      Brush.Color:=BACKGROUND;
      Brush.Color:=LINES_COLOR;
      Brush.Style:=bsCross;
      Pen.Color:=BACKGROUND;
      buf.Canvas.Rectangle(0,0,Buf.Width,Buf.Height);

      { рисование }
      w:=Buf.Width;
      RecsLeftBord:=Border+2*TimeLeftBord+TextWidth('88:88');
      CurTop:=Border;

      { заполнение }
      for i:=0 to high(Page.Chron) do
      begin
        k:=Page.Chron[i].RecNumber;
        case Page.Chron[i].RecType of
          rtBlood: AddPanel(
                     i,
                     PANEL_COLORS[rtBlood],
                     Page.Blood[k].Time,
                     [RealToStrZero(Page.Blood[k].Value)],
                     [fsBold]
                   );
          rtIns: begin
                   AddPanel(
                     i,
                     PANEL_COLORS[rtIns],
                     Page.Ins[k].Time,
                     ['[-'+IntToStr(Page.Ins[k].Value)+']'],
                     []
                   );
                 end;
          rtMeal: begin
                    AddPanel(
                      i,
                      PANEL_COLORS[rtMeal],
                      Page.Meal[k].Time,
                      CreateRecsArray(Page.Meal[k]),
                      []
                    );
                  end;
        end;
      end;
    end;
    Image.Canvas.Draw(0,0,buf);
  finally
    buf.Free;
  end;
end;

{=========================================================}
procedure EraseDiaryPage(Image: TImage);
{=========================================================}
begin
  Image.Align:=alClient;
  Image.Picture.Bitmap.Width:=Image.Width;
  Image.Picture.Bitmap.Height:=Image.Height;
  //Image.Height:=20;
  Image.Canvas.Pen.Color:=BACKGROUND;

  Image.Canvas.Brush.Color:=BACKGROUND;
  Image.Canvas.Brush.Style:=bsSolid;
  Image.Canvas.Rectangle(0,0,Image.Width,Image.Height);

  Image.Canvas.Brush.Color:=LINES_COLOR;
  Image.Canvas.Brush.Style:=bsCross;
  Image.Canvas.Rectangle(0,0,Image.Width,Image.Height);
  Image.Align:=alTop;
end;

{ САМАЯ СУРОВАЯ ПРОЦЕДУРА В ГРАФИЧЕСКОМ РЕЖИМЕ ]:-> }

{=========================================================}
procedure GetMousePlace(x,y: integer; const Page: TDiaryPage; var ClickInfo: TClickInfo);
{=========================================================}

  function InRect(r: TRect): boolean;
  begin
    result:=PtInRect(r,Point(x,y));
  end;

var
  i,j: integer;
begin

  ClickInfo.PlaceType:=cpNoWhere;
  ClickInfo.PanelNumber:=-1;
  ClickInfo.LineNumber:=-1;
  ClickInfo.RecNumber:=-1;

  { определяем место курсора }
  for i:=0 to high(PanelRects) do
  if InRect(PanelRects[i].Rect) then
  begin
    ClickInfo.PanelNumber:=i;
    ClickInfo.PlaceType:=cpPanel;
    ClickInfo.RecordType:=Page.Chron[PanelRects[i].cNumb].RecType;
    ClickInfo.RecNumber:=Page.Chron[PanelRects[i].cNumb].RecNumber;

    if (InRect(PanelRects[i].TimeRect))then
    begin
      ClickInfo.PlaceType:=cpTime;
      exit;
    end else

    for j:=0 to high(PanelRects[i].Recs) do
    if InRect(PanelRects[i].Recs[j]) then
    begin
      ClickInfo.LineNumber:=j;
      ClickInfo.PlaceType:=cpRec;
      exit;
    end;
  end;
end;

{=========================================================}
procedure ProcessClick(
  x,y: integer;
  Button: TMouseButton;
  Image: TImage;
  var Page: TDiaryPage;
  const Diary: TDiary;
  PopupDish,PopupPanel: TPopupMenu);
{=========================================================}
var
  DoubleClick: boolean;
  Cursor: TPoint;

  ClickInfo: TClickInfo;
begin
  { определим, является ли клик двойным }
  DoubleClick:=
    ((GetTickCount-LastClickedTime)<DoubleClickTime)and
    (abs(X-LastX)+abs(Y-LastY)<DoubleClickShift);
  LastClickedTime:=GetTickCount;
  LastX:=x;
  LastY:=y;

  GetMousePlace(x,y,Page,ClickInfo);

  case ClickInfo.PlaceType of
    cpPanel:  begin
                if Button=mbRight then
                begin
                  //if ClickInfo.RecordType=rtMeal then
                  SelectedPanel:=ClickInfo.PanelNumber;
                  //else
                  //  SelectedPanel:=-1;
                  SelectedLine:=-1;
                  DrawDiaryPage(Image,Page);
                  GetCursorPos(Cursor);
                  PopupPanel.Popup(Cursor.x,Cursor.y);
                end else
                if ClickInfo.RecordType=rtMeal then
                begin
                  ShowMealResult(
                    Page.Meal[ClickInfo.RecNumber],
                    SearchInjectedDose(Page,
                      Page.Meal[ClickInfo.RecNumber].Time));
                  SelectedPanel:=ClickInfo.PanelNumber;
                  SelectedLine:=-1;
                  DrawDiaryPage(Image,Page);
                end else
                begin
                  SelectedPanel:=-1;
                  SelectedLine:=-1;
                  DrawDiaryPage(Image,Page);
                end;
              end;
    cpTime:   if DoubleClick then
                ChangeTime(Page,Diary,ClickInfo.RecordType,ClickInfo.RecNumber,Image);

    cpRec:    if ClickInfo.RecordType=rtMeal then
              begin
                SelectedPanel:=ClickInfo.PanelNumber;
                SelectedLine:=ClickInfo.LineNumber;
                ShowMealResult(
                  Page.Meal[ClickInfo.RecNumber],
                  SearchInjectedDose(Page,Page.Meal[ClickInfo.RecNumber].Time));
                DrawDiaryPage(Image,Page);
                if Button=mbRight then
                begin
                  SelMeal:=ClickInfo.RecNumber;
                  GetCursorPos(Cursor);
                  PopupDish.Popup(Cursor.x,Cursor.y);
                end else
                if DoubleClick then
                begin
                  DialogChangeDishMass(Page,ClickInfo.RecNumber,ClickInfo.LineNumber);
                  DrawDiaryPage(Image,Page);
                  SaveDiary(Diary);
                end;
              end;

    cpNoWhere:begin
                SelectedPanel:=-1;
                SelectedLine:=-1;
                DrawDiaryPage(Image,Page);
                ClearMealResult;
              end;
  end;
end;

{=========================================================}
procedure ProcessMove(x,y: integer; Image: TImage; const Page: TDiaryPage);
{=========================================================}
var
  MouseInfo: TClickInfo;
begin
  GetMousePlace(x,y,Page,MouseInfo);

  if (MouseInfo.PlaceType=cpRec)and(MouseInfo.RecordType=rtMeal) then
    Image.Cursor:=crHandPoint
  else
    Image.Cursor:=crDefault;
end;  *)

{=========================================================}
procedure ChangeTime(var Page: TDiaryPage; const Diary: TDiary;
  RecType: TRecType; N: integer; BackImage: TImage);
{=========================================================}
var
  NewTime: integer;
  old,s: string;
begin
  //двойной клик по часам
  {!!! нет проверки N }
  case RecType of
    rtBlood: s:=TimeToStr(Page.Blood[N].Time);
    rtIns: s:=TimeToStr(Page.Ins[N].Time);
    rtMeal: s:=TimeToStr(Page.Meal[N].Time);
  end;
  old:=s;
  s:=InputBox('Изменить время','Новое время:',s);
  if s<>old then
  begin
    if not StrToTime(s,NewTime) then
      ErrorMessage('Неверное время') else
    begin
      case RecType of
        rtBlood: Page.Blood[N].Time:=NewTime;
        rtIns: Page.Ins[N].Time:=NewTime;
        rtMeal: Page.Meal[N].Time:=NewTime;
      end;
      //UpdateChron(Page);
      //DrawDiaryPage(BackImage,Page);
      //SaveDiary(Diary);
    end;
  end;
end;

end.
