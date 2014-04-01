package org.bosik.diacomp.core.services.diary;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;

/**
 * Diary records service
 *
 * @author Bosik
 */
public interface DiaryService extends ObjectService<DiaryRecord>
{
	/**
	 * Returns list of non-deleted records for the specified time interval
	 *
	 * @param fromTime
	 * @param toTime
	 * @param includeRemoved
	 * @return
	 * @throws CommonServiceException
	 */
	List<Versioned<DiaryRecord>> findBetween(Date fromTime, Date toTime, boolean includeRemoved)
			throws CommonServiceException;
}
