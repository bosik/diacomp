unit JsonVersionedSerializer;

interface

uses
  SysUtils,
  DiaryRoutines,
  uLKJson,
  BusinessObjects;

  procedure ReadVersioned(json: TlkJSONobject; Dest: TVersioned);
  procedure WriteVersioned(Src: TVersioned; json: TlkJSONobject);

const
  REC_ID              = 'id';
  REC_TIMESTAMP       = 'stamp';
  REC_VERSION         = 'version';
  REC_DELETED         = 'deleted';
  REC_DATA            = 'data';

implementation

{==============================================================================}
procedure ReadVersioned(json: TlkJSONobject; Dest: TVersioned);
{==============================================================================}
//var
//  JsonObj: TlkJSONobject;
begin
  //JsonObj := json as TlkJSONobject;
  Dest.ID := (json[REC_ID] as TlkJSONstring).Value;
  Dest.TimeStamp := StrToDateTime((json[REC_TIMESTAMP] as TlkJSONstring).Value, STD_DATETIME_FMT);
  Dest.Version := (json[REC_VERSION] as TlkJSONnumber).Value;
  Dest.Deleted := (json[REC_DELETED] as TlkJSONboolean).Value;
end;

{==============================================================================}
procedure WriteVersioned(Src: TVersioned; json: TlkJSONobject);
{==============================================================================}
begin
  json.Add(REC_ID, Src.ID);
  json.Add(REC_TIMESTAMP, DateTimeToStr(Src.TimeStamp, STD_DATETIME_FMT));
  json.Add(REC_VERSION, Src.Version);
  json.Add(REC_DELETED, Src.Deleted);
end;

end.
