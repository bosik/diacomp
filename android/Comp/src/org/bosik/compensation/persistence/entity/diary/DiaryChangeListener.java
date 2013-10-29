package org.bosik.compensation.persistence.entity.diary;

import org.bosik.compensation.persistence.entity.diary.records.DiaryRecord;

@Deprecated
public interface DiaryChangeListener
{
	public static enum EventType
	{
		ADD, MODIFY, REMOVE
	}

	public void changed(EventType eventType, Class<? extends DiaryRecord> recClass, DiaryRecord recInstance);
}
