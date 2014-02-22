package org.bosik.diacomp.features.diary.dao;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.bo.diary.DiaryRecord;
import org.bosik.diacomp.core.persistence.common.Versioned;

public interface DiaryDAO
{
	// FIXME: pass list of GUIDs
	List<Versioned<String>> findGuid(int userId, String guid);

	List<Versioned<String>> findMod(int userId, Date time, boolean includeRemoved);

	List<Versioned<String>> findPeriod(int userId, Date startTime, Date endTime, boolean includeRemoved);

	void post(int userId, List<Versioned<DiaryRecord>> records);
}
