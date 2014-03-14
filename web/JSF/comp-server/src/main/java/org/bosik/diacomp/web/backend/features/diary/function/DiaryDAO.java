package org.bosik.diacomp.web.backend.features.diary.function;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;

public interface DiaryDAO
{
	Versioned<String> findByGuid(int userId, String guid);

	List<Versioned<String>> findChanged(int userId, Date since);

	List<Versioned<String>> findPeriod(int userId, Date startTime, Date endTime, boolean includeRemoved);

	void post(int userId, List<Versioned<DiaryRecord>> records);
}
