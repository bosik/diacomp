unit TextInterface;

interface

// TODO: ������������� ��������
// TODO: ����� �����

resourcestring
  { ��������� ���� }
  PROGRAM_TITLE                     = '�����������';

  { ��������� \ ������ }
  MESSAGE_ERROR_NO_INTERNET         = '��� ����������� � ���� ��������.';
  MESSAGE_ERROR_INITIALIZATION      = '�������� ����������� ������ (���� "%s": %s). �������� ��������.';
  //MESSAGE_ERROR_INPUT_INT           = '�������� ��������. ������� ����� �����.';
  MESSAGE_ERROR_INPUT_INT_POSITIVE  = '�������� ��������. ������� ����� ������������� �����.';
  MESSAGE_ERROR_INPUT_REAL          = '�������� ��������. ������� ������������ �����.';
  //MESSAGE_ERROR_CANT_CREATE_DIARY = '���������� ������� ���� ��������';
  //MESSAGE_ERROR_LOADINGDIARY      = '������ �������� ��������';
  //MESSAGE_ERROR_LOADINGFOODBASE   = '������ �������� ���� ���������';
  //MESSAGE_ERROR_LOADINGDISHBASE   = '������ �������� ���� ����';

  { ��������� \ ���������� }
  MESSAGE_INFO_NO_UPDATES           = '�� ����������� ����� ��������� ������ ���������.';
  MESSAGE_INFO_CANT_BALANCE         = '����� ��������� �� 0, �� ��� ������������ ���� ���� ���� ��� ���-�� ������.';

  { ��������� \ ������� }
  MESSAGE_CONF_FIRST_WARNING  = '��������! ����� ��������� '+
    '�� ����� ��������������� �� ����� �����, ����� ��� '+
    '�������� ��������� � �������������� ������ ���������. '+
    '��� ���������� ������ ��������� ����� '+
    '���������������� ��������. �� ��������� � ������� '+
    '������. �� ���� �������� ������������ ������ �� ����.'#13#13+
    '���� �� ���������� ��� �������, ������� "��" (Yes)'#13+
    '���� �� ���������� - ������� "���" (No).';
const
  MESSAGE_CONF_REMOVE_RECORD: array[1..4] of string = (
    '������� ����� ��?',
    '������� ��������?',
    '������� ���� ����?',
    '������� �������?'
  );
  MESSAGE_CONF_UPDATE: array[Boolean] of string = (
    '�������� ����� ������ ���������. ���������� ������?',
    '���������� ���������� ������?'
  );

resourcestring
  MESSAGE_CONF_REMOVE_DIARY_FOOD = '������� <%s> ?';
  MESSAGE_CONF_REMOVE_FOOD       = '������� ������� <%s>?';
  MESSAGE_CONF_REMOVE_FOOD_USED  = '������� <%s> ������������ � ����� <%s>.'#13'�� ����� �������?';
  MESSAGE_CONF_REMOVE_DISH       = '������� ����� <%s>?';
  MESSAGE_CONF_REMOVE_DISH_USED  = '����� <%s> ������������ � ����� <%s>.'#13'�� ����� �������?';

  { ����������� \ ���������� }
  BALLOON_INFO_NEW_VERSION_AVAILABLE  = '�������� ����� ������ ���������. ��� ��������� ����� ��������� ���������� ������� �� ��� ���������.';
  BALOON_ERROR_ANALYZER_NOT_FOUNDED   = '������: ������ ������� �� ��������';     

const
  { ������ }
  SAVE_CAPTION: array[Boolean] of string = ('���������','�������');

resourcestring
  { ������ ��������� }
  STATUS_ACTION_LOADING_DIARY        = '�������� ��������';
  STATUS_ACTION_LOADING_GRAPHICS     = '�������� �������';
  STATUS_ACTION_WEB_SETUP            = '��������� ���-�������';
  STATUS_ACTION_APPLYING_SETTINGS    = '���������� ���������������� ��������';
  STATUS_ACTION_PREPARING_INFOPANELS = '��������� �������������� �������';
  STATUS_ACTION_DOWNLOADING_FOODBASE = '��������� ���� ���������';
  STATUS_ACTION_DOWNLOADING_DISHBASE = '��������� ���� ����';
  STATUS_ACTION_CONVERT_FOODBASE     = '��������������� ���� ���������';
  STATUS_ACTION_CONVERT_DISHBASE     = '��������������� ���� ����';
  STATUS_ACTION_LOADING_FOODBASE     = '�������� ���� ���������';
  STATUS_ACTION_LOADING_DISHBASE     = '�������� ���� ����';
  STATUS_ACTION_AUTH                 = '�����������';
  STATUS_ACTION_SYNC_DIARY           = '������������� ��������';
  STATUS_ACTION_SYNC_FOODBASE        = '������������� ���� ���������';
  STATUS_ACTION_SYNC_DISHBASE        = '������������� ���� ����';
  STATUS_ACTION_LOADING_MATHAN       = '�������� ������ �������';
  STATUS_ACTION_PREPARING_KOOFS      = '������ ������';
  STATUS_ACTION_UPLOADING_KOOFS      = '�������� ������������� �� ������'; 
  STATUS_READY                       = '������';
  STATUS_LOADING_TIME                = '����� �������: %d ����';
  STATUS_SYNC_DONE                   = '������� ���������������';

  STATUS_STATE_ONLINE                = '������';
  STATUS_STATE_OFFLINE               = '�������';

  { ����� }
  HINT_FINGER_NOT_FOUND = '���������� ����� �� �� ������';

  { ������� �� ����������� }
  LABEL_FINGER           = '�����:';
  LABEL_AFTER_MEAL       = '����� ���:';
  LABEL_AFTER_INS        = '����� �����:';
  MESSAGE_SELECT_MEAL    = '�������� ���� ���� ��� �������� �����';
const
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
