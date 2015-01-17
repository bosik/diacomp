unit TextInterface;

interface

uses
  Classes, SysUtils, DiaryRoutines;

var
  APPLICATION_TITLE                   : string = '�����������';

  BALLOON_INFO_NEW_VERSION_AVAILABLE  : string = '�������� ����� ������ ���������. ��� ��������� ����� ��������� ���������� ������� �� ��� ���������.';
  BALLOON_ERROR_ANALYZER_NOT_FOUNDED  : string = '������: ������ ������� �� ��������';
  MAIN_BASES                               : string;
  MAIN_BASES_FOOD_TITLE                    : string;
  MAIN_BASES_DISH_TITLE                    : string;
  MAIN_DIARY                               : string;
  MAIN_DIARY_PANEL_TIME_FINGER_NOT_FOUND   : string = '���������� ����� �� �� ������';
  MAIN_DIARY_PANEL_TIME_FINGER             : string = '�����:';
  MAIN_DIARY_PANEL_TIME_AFTER_MEAL         : string = '����� ���:';
  MAIN_DIARY_PANEL_TIME_AFTER_INS          : string = '����� �����:';
  MAIN_DIARY_PANEL_ADD_SELECT_MEAL         : string = '�������� ���� ���� ��� �������� �����';
  MAIN_DIARY_VIEW_EMPTYPAGE: string;
  MESSAGE_CONF_FIRST_WARNING  : string = '��������! ����� ��������� '+
    '�� ����� ��������������� �� ����� �����, ����� ��� '+
    '�������� ��������� � �������������� ������ ���������. '+
    '��� ���������� ������ ��������� ����� '+
    '���������������� ��������. �� ��������� � ������� '+
    '������. �� ���� �������� ������������ ������ �� ����.'#13#13+
    '���� �� ���������� ��� �������, ������� "��" (Yes)'#13+
    '���� �� ���������� - ������� "���" (No).';
  MESSAGE_CONF_REMOVE_DIARY_UNKNOWN   : string = '������� ������?';
  MESSAGE_CONF_REMOVE_DIARY_BLOOD     : string = '������� ����� ��?';
  MESSAGE_CONF_REMOVE_DIARY_INS       : string = '������� ��������?';
  MESSAGE_CONF_REMOVE_DIARY_MEAL      : string = '������� ���� ����?';
  MESSAGE_CONF_REMOVE_DIARY_NOTE      : string = '������� �������?';
  MESSAGE_CONF_REMOVE_DIARY_FOOD      : string = '������� <%s> ?';
  MESSAGE_CONF_REMOVE_FOOD            : string = '������� ������� <%s>?';
  MESSAGE_CONF_REMOVE_FOOD_USED       : string = '������� <%s> ������������ � ����� <%s>.'#13'�� ����� �������?';
  MESSAGE_CONF_REMOVE_DISH            : string = '������� ����� <%s>?';
  MESSAGE_CONF_REMOVE_DISH_USED       : string = '����� <%s> ������������ � ����� <%s>.'#13'�� ����� �������?';
  MESSAGE_ERROR_NO_INTERNET           : string = '��� ����������� � ���� ��������.';
  MESSAGE_ERROR_INITIALIZATION        : string = '�������� ����������� ������ (���� "%s": %s). �������� ��������.';
  MESSAGE_ERROR_INPUT_INT_POSITIVE    : string = '�������� ��������. ������� ����� ������������� �����.';
  MESSAGE_ERROR_INPUT_REAL            : string = '�������� ��������. ������� ������������ �����.';
  MESSAGE_INFO_NO_UPDATES             : string = '�� ����������� ����� ��������� ������ ���������.';
  MESSAGE_INFO_CANT_BALANCE           : string = '����� ��������� �� 0, �� ��� ������������ ���� ���� ���� ��� ���-�� ������.';
  STATUS_ACTION_LOADING_DIARY         : string = '�������� ��������';
  STATUS_ACTION_LOADING_GRAPHICS      : string = '�������� �������';
  STATUS_ACTION_WEB_SETUP             : string = '��������� ���-�������';
  STATUS_ACTION_APPLYING_SETTINGS     : string = '���������� ���������������� ��������';
  STATUS_ACTION_PREPARING_INFOPANELS  : string = '��������� �������������� �������';
  STATUS_ACTION_DOWNLOADING_FOODBASE  : string = '��������� ���� ���������';
  STATUS_ACTION_DOWNLOADING_DISHBASE  : string = '��������� ���� ����';
  STATUS_ACTION_CONVERT_FOODBASE      : string = '��������������� ���� ���������';
  STATUS_ACTION_CONVERT_DISHBASE      : string = '��������������� ���� ����';
  STATUS_ACTION_LOADING_FOODBASE      : string = '�������� ���� ���������';
  STATUS_ACTION_LOADING_DISHBASE      : string = '�������� ���� ����';
  STATUS_ACTION_AUTH                  : string = '�����������';
  STATUS_ACTION_SYNC_DIARY            : string = '������������� ��������';
  STATUS_ACTION_SYNC_FOODBASE         : string = '������������� ���� ���������';
  STATUS_ACTION_SYNC_DISHBASE         : string = '������������� ���� ����';
  STATUS_ACTION_LOADING_MATHAN        : string = '�������� ������ �������';
  STATUS_ACTION_PREPARING_KOOFS       : string = '������ ������';
  STATUS_ACTION_UPLOADING_KOOFS       : string = '�������� ������������� �� ������';
  STATUS_RESULT_READY                 : string = '������';
  STATUS_RESULT_LOADING_TIME          : string = '����� �������: %d ����';
  STATUS_RESULT_SYNC_DONE             : string = '������� ���������������';
  STATUS_RESULT_STATE_ONLINE          : string = '������';
  STATUS_RESULT_STATE_OFFLINE         : string = '�������';

const
  MESSAGE_CONF_UPDATE: array[Boolean] of string = (
    '�������� ����� ������ ���������. ���������� ������?',
    '���������� ���������� ������?'
  );
  { ������ }
  SAVE_CAPTION: array[Boolean] of string = ('���������','�������');

  KoofDisc: array[0..3] of string = (
    '����������, ��������� ������������� �� ��� �������� 1� ���������',
    '����������, ��������� ����������� �� ��� �������� 1 �� �������� ��������',
    '����������, ��������� ������������� �� ��� �������� 1� ������',
    '����������, ������� �������� ��������� ��� �������� 1� ��������� � 0,25� ������');

  { ������ }
  LongFingerNames: array[0..9] of string = (
    '[�����]: �������',
    '[�����]: ������������',
    '[�����]: �������',
    '[�����]: ����������',
    '[�����]: �������',
    '[������]: �������',
    '[������]: ����������',
    '[������]: �������',
    '[������]: ������������',
    '[������]: �������'
  );

  ShortFingerNames: array[0..9] of string = (
    '��',
    '1�',
    '2�',
    '3�',
    '4�',
    '4�',
    '3�',
    '2�',
    '1�',
    '��'
  ); 

  MiddleFingerNames: array[0..9] of string = (
    '< �������',
    '< ������.',
    '< �������',
    '< �������.',
    '< �������',
    '������� >',
    '�������. >',
    '������� >',
    '������. >',
    '������� >'
  );

  procedure LoadStringResources(const FileName: string);

implementation

{==============================================================================}
procedure LoadStringResources(const FileName: string);
{==============================================================================}

  function GetValue(const S: string): string;
  begin
    Result := Trim(TextAfter(S, '='));
  end;

var
  Map: TStringMap;
    
  function Extract(const Name: string): string;
  begin
    try
      Result := GetValue(Map[Name]);
    except
      on EKeyNotFoundException do
        raise Exception.Create(Format('Resource string "%s" not founded', [Name]));
    end;
  end;

begin
  Map := TStringMap.Create;
  try
    Map.LoadFromFile(FileName);

    APPLICATION_TITLE                   := Extract('APPLICATION_TITLE');
    BALLOON_INFO_NEW_VERSION_AVAILABLE  := Extract('BALLOON_INFO_NEW_VERSION_AVAILABLE');
    BALLOON_ERROR_ANALYZER_NOT_FOUNDED  := Extract('BALLOON_ERROR_ANALYZER_NOT_FOUNDED');
    MAIN_BASES                               := Extract('MAIN_BASES');
    MAIN_BASES_FOOD_TITLE                    := Extract('MAIN_BASES_FOOD_TITLE');
    MAIN_BASES_DISH_TITLE                    := Extract('MAIN_BASES_DISH_TITLE');
    MAIN_DIARY                               := Extract('MAIN_DIARY'); 
    MAIN_DIARY_PANEL_TIME_FINGER_NOT_FOUND   := Extract('MAIN_DIARY_PANEL_TIME_FINGER_NOT_FOUND');
    MAIN_DIARY_PANEL_TIME_FINGER             := Extract('MAIN_DIARY_PANEL_TIME_FINGER');
    MAIN_DIARY_PANEL_TIME_AFTER_MEAL         := Extract('MAIN_DIARY_PANEL_TIME_AFTER_MEAL');
    MAIN_DIARY_PANEL_TIME_AFTER_INS          := Extract('MAIN_DIARY_PANEL_TIME_AFTER_INS');
    MAIN_DIARY_PANEL_ADD_SELECT_MEAL         := Extract('MAIN_DIARY_PANEL_ADD_SELECT_MEAL');
    MESSAGE_CONF_FIRST_WARNING          := Extract('MESSAGE_CONF_FIRST_WARNING');
    MESSAGE_CONF_REMOVE_DIARY_UNKNOWN   := Extract('MESSAGE_CONF_REMOVE_DIARY_UNKNOWN');
    MESSAGE_CONF_REMOVE_DIARY_BLOOD     := Extract('MESSAGE_CONF_REMOVE_DIARY_BLOOD');
    MESSAGE_CONF_REMOVE_DIARY_INS       := Extract('MESSAGE_CONF_REMOVE_DIARY_INS');
    MESSAGE_CONF_REMOVE_DIARY_MEAL      := Extract('MESSAGE_CONF_REMOVE_DIARY_MEAL');
    MESSAGE_CONF_REMOVE_DIARY_NOTE      := Extract('MESSAGE_CONF_REMOVE_DIARY_NOTE');
    MESSAGE_CONF_REMOVE_DIARY_FOOD      := Extract('MESSAGE_CONF_REMOVE_DIARY_FOOD');
    MESSAGE_CONF_REMOVE_FOOD            := Extract('MESSAGE_CONF_REMOVE_FOOD');
    MESSAGE_CONF_REMOVE_FOOD_USED       := Extract('MESSAGE_CONF_REMOVE_FOOD_USED');
    MESSAGE_CONF_REMOVE_DISH            := Extract('MESSAGE_CONF_REMOVE_DISH');
    MESSAGE_CONF_REMOVE_DISH_USED       := Extract('MESSAGE_CONF_REMOVE_DISH_USED');
    MESSAGE_ERROR_NO_INTERNET           := Extract('MESSAGE_ERROR_NO_INTERNET');
    MESSAGE_ERROR_INITIALIZATION        := Extract('MESSAGE_ERROR_INITIALIZATION');
    MESSAGE_ERROR_INPUT_INT_POSITIVE    := Extract('MESSAGE_ERROR_INPUT_INT_POSITIVE');
    MESSAGE_ERROR_INPUT_REAL            := Extract('MESSAGE_ERROR_INPUT_REAL');
    MESSAGE_INFO_NO_UPDATES             := Extract('MESSAGE_INFO_NO_UPDATES');
    MESSAGE_INFO_CANT_BALANCE           := Extract('MESSAGE_INFO_CANT_BALANCE');
    STATUS_ACTION_LOADING_DIARY         := Extract('STATUS_ACTION_LOADING_DIARY');
    STATUS_ACTION_LOADING_GRAPHICS      := Extract('STATUS_ACTION_LOADING_GRAPHICS');
    STATUS_ACTION_WEB_SETUP             := Extract('STATUS_ACTION_WEB_SETUP');
    STATUS_ACTION_APPLYING_SETTINGS     := Extract('STATUS_ACTION_APPLYING_SETTINGS');
    STATUS_ACTION_PREPARING_INFOPANELS  := Extract('STATUS_ACTION_PREPARING_INFOPANELS');
    STATUS_ACTION_DOWNLOADING_FOODBASE  := Extract('STATUS_ACTION_DOWNLOADING_FOODBASE');
    STATUS_ACTION_DOWNLOADING_DISHBASE  := Extract('STATUS_ACTION_DOWNLOADING_DISHBASE');
    STATUS_ACTION_CONVERT_FOODBASE      := Extract('STATUS_ACTION_CONVERT_FOODBASE');
    STATUS_ACTION_CONVERT_DISHBASE      := Extract('STATUS_ACTION_CONVERT_DISHBASE');
    STATUS_ACTION_LOADING_FOODBASE      := Extract('STATUS_ACTION_LOADING_FOODBASE');
    STATUS_ACTION_LOADING_DISHBASE      := Extract('STATUS_ACTION_LOADING_DISHBASE');
    STATUS_ACTION_AUTH                  := Extract('STATUS_ACTION_AUTH');
    STATUS_ACTION_SYNC_DIARY            := Extract('STATUS_ACTION_SYNC_DIARY');
    STATUS_ACTION_SYNC_FOODBASE         := Extract('STATUS_ACTION_SYNC_FOODBASE');
    STATUS_ACTION_SYNC_DISHBASE         := Extract('STATUS_ACTION_SYNC_DISHBASE');
    STATUS_ACTION_LOADING_MATHAN        := Extract('STATUS_ACTION_LOADING_MATHAN');
    STATUS_ACTION_PREPARING_KOOFS       := Extract('STATUS_ACTION_PREPARING_KOOFS');
    STATUS_ACTION_UPLOADING_KOOFS       := Extract('STATUS_ACTION_UPLOADING_KOOFS');
    STATUS_RESULT_READY                 := Extract('STATUS_RESULT_READY');
    STATUS_RESULT_LOADING_TIME          := Extract('STATUS_RESULT_LOADING_TIME');
    STATUS_RESULT_SYNC_DONE             := Extract('STATUS_RESULT_SYNC_DONE');
    STATUS_RESULT_STATE_ONLINE          := Extract('STATUS_RESULT_STATE_ONLINE');
    STATUS_RESULT_STATE_OFFLINE         := Extract('STATUS_RESULT_STATE_OFFLINE');
    MAIN_DIARY_VIEW_EMPTYPAGE := Extract('MAIN_DIARY_VIEW_EMPTYPAGE');
  finally
    Map.Free;
  end;
end;    

end.

