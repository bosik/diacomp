package org.bosik.diacomp.features.diary.dao;

import java.util.Date;
import java.util.List;

import org.bosik.diacomp.persistence.common.Versioned;

public interface DiaryDAO
{
	List<Versioned<String>> findMod(int userId, Date time, boolean includeRemoved);

	List<Versioned<String>> findPeriod(int userId, Date startTime, Date endTime, boolean includeRemoved);
}
