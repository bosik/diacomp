package org.bosik.diacomp.web.backend.features.diary.function;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;

public interface DiaryDAO
{
	void delete(int userId, String id);

	Versioned<DiaryRecord> findByGuid(int userId, String guid);

	List<Versioned<DiaryRecord>> findChanged(int userId, Date since);

	List<Versioned<DiaryRecord>> findPeriod(int userId, Date startTime, Date endTime, boolean includeRemoved);

	void post(int userId, List<Versioned<DiaryRecord>> records);
}