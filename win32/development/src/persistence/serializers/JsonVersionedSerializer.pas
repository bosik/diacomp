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
  REC_HASH            = 'hash';
  REC_TIMESTAMP       = 'stamp';
  REC_VERSION         = 'version';
  REC_DELETED         = 'deleted';
  REC_DATA            = 'data';

implementation

{======================================================================================================================}
procedure ReadVersioned(json: TlkJSONobject; Dest: TVersioned);
{======================================================================================================================}
begin
  Dest.ID := (json[REC_ID] as TlkJSONstring).Value;
  Dest.Hash := (json[REC_HASH] as TlkJSONstring).Value;
  Dest.TimeStamp := ParseDateTime((json[REC_TIMESTAMP] as TlkJSONstring).Value);
  Dest.Version := (json[REC_VERSION] as TlkJSONnumber).Value;
  Dest.Deleted := (json[REC_DELETED] as TlkJSONboolean).Value;
end;

{======================================================================================================================}
procedure WriteVersioned(Src: TVersioned; json: TlkJSONobject);
{======================================================================================================================}
begin
  json.Add(REC_ID, Src.ID);
  json.Add(REC_HASH, Src.Hash);
  json.Add(REC_TIMESTAMP, FormatDateTime(Src.TimeStamp));
  json.Add(REC_VERSION, Src.Version);
  json.Add(REC_DELETED, Src.Deleted);
end;

end.
