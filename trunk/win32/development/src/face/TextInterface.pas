unit TextInterface;

interface

resourcestring
  APPLICATION_TITLE                   = '�����������';

  { ����������� ��������� }
  BALLOON_INFO_NEW_VERSION_AVAILABLE  = '�������� ����� ������ ���������. ��� ��������� ����� ��������� ���������� ������� �� ��� ���������.';
  BALLOON_ERROR_ANALYZER_NOT_FOUNDED  = '������: ������ ������� �� ��������';

  { ���� }
  BASES_FILTER_ALL                    = '�������� ��� ������';
  BASES_FILTER_FILTERED               = '������ ������ (Escape)';

  { ������� }
  DIARY_PANEL_TIME_FINGER_NOT_FOUND   = '���������� ����� �� �� ������';
  DIARY_PANEL_TIME_FINGER             = '�����:';
  DIARY_PANEL_TIME_AFTER_MEAL         = '����� ���:';
  DIARY_PANEL_TIME_AFTER_INS          = '����� �����:';
  DIARY_PANEL_ADD_SELECT_MEAL         = '�������� ���� ���� ��� �������� �����';

  { ��������� }
  MESSAGE_CONF_FIRST_WARNING  = '��������! ����� ��������� '+
    '�� ����� ��������������� �� ����� �����, ����� ��� '+
    '�������� ��������� � �������������� ������ ���������. '+
    '��� ���������� ������ ��������� ����� '+
    '���������������� ��������. �� ��������� � ������� '+
    '������. �� ���� �������� ������������ ������ �� ����.'#13#13+
    '���� �� ���������� ��� �������, ������� "��" (Yes)'#13+
    '���� �� ���������� - ������� "���" (No).';
  MESSAGE_CONF_REMOVE_DIARY_UNKNOWN   = '������� ������?';
  MESSAGE_CONF_REMOVE_DIARY_BLOOD     = '������� ����� ��?';
  MESSAGE_CONF_REMOVE_DIARY_INS       = '������� ��������?';
  MESSAGE_CONF_REMOVE_DIARY_MEAL      = '������� ���� ����?';
  MESSAGE_CONF_REMOVE_DIARY_NOTE      = '������� �������?';
  MESSAGE_CONF_REMOVE_DIARY_FOOD      = '������� <%s> ?';
  MESSAGE_CONF_REMOVE_FOOD            = '������� ������� <%s>?';
  MESSAGE_CONF_REMOVE_FOOD_USED       = '������� <%s> ������������ � ����� <%s>.'#13'�� ����� �������?';
  MESSAGE_CONF_REMOVE_DISH            = '������� ����� <%s>?';
  MESSAGE_CONF_REMOVE_DISH_USED       = '����� <%s> ������������ � ����� <%s>.'#13'�� ����� �������?';
  MESSAGE_ERROR_NO_INTERNET           = '��� ����������� � ���� ��������.';
  MESSAGE_ERROR_INITIALIZATION        = '�������� ����������� ������ (���� "%s": %s). �������� ��������.';
  MESSAGE_ERROR_INPUT_INT_POSITIVE    = '�������� ��������. ������� ����� ������������� �����.';
  MESSAGE_ERROR_INPUT_REAL            = '�������� ��������. ������� ������������ �����.';
  MESSAGE_INFO_NO_UPDATES             = '�� ����������� ����� ��������� ������ ���������.';
  MESSAGE_INFO_CANT_BALANCE           = '����� ��������� �� 0, �� ��� ������������ ���� ���� ���� ��� ���-�� ������.';
  
  { ������ ��������� }
  STATUS_ACTION_LOADING_DIARY         = '�������� ��������';
  STATUS_ACTION_LOADING_GRAPHICS      = '�������� �������';
  STATUS_ACTION_WEB_SETUP             = '��������� ���-�������';
  STATUS_ACTION_APPLYING_SETTINGS     = '���������� ���������������� ��������';
  STATUS_ACTION_PREPARING_INFOPANELS  = '��������� �������������� �������';
  STATUS_ACTION_DOWNLOADING_FOODBASE  = '��������� ���� ���������';
  STATUS_ACTION_DOWNLOADING_DISHBASE  = '��������� ���� ����';
  STATUS_ACTION_CONVERT_FOODBASE      = '��������������� ���� ���������';
  STATUS_ACTION_CONVERT_DISHBASE      = '��������������� ���� ����';
  STATUS_ACTION_LOADING_FOODBASE      = '�������� ���� ���������';
  STATUS_ACTION_LOADING_DISHBASE      = '�������� ���� ����';
  STATUS_ACTION_AUTH                  = '�����������';
  STATUS_ACTION_SYNC_DIARY            = '������������� ��������';
  STATUS_ACTION_SYNC_FOODBASE         = '������������� ���� ���������';
  STATUS_ACTION_SYNC_DISHBASE         = '������������� ���� ����';
  STATUS_ACTION_LOADING_MATHAN        = '�������� ������ �������';
  STATUS_ACTION_PREPARING_KOOFS       = '������ ������';
  STATUS_ACTION_UPLOADING_KOOFS       = '�������� ������������� �� ������';
  STATUS_RESULT_READY                 = '������';
  STATUS_RESULT_LOADING_TIME          = '����� �������: %d ����';
  STATUS_RESULT_SYNC_DONE             = '������� ���������������';
  STATUS_RESULT_STATE_ONLINE          = '������';
  STATUS_RESULT_STATE_OFFLINE         = '�������';

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
  
implementation

end.
