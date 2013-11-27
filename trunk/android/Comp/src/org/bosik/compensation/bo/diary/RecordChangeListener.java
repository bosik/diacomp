package org.bosik.compensation.bo.diary;

import org.bosik.compensation.bo.diary.records.DiaryRecord;

@Deprecated
public interface RecordChangeListener
{
	public void changed(Class<? extends DiaryRecord> recClass, DiaryRecord recInstance);
}
