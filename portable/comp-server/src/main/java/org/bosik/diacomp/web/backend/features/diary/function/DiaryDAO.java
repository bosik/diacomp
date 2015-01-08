package org.bosik.diacomp.web.backend.features.diary.function;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;

public interface DiaryDAO
{
	void delete(int userId, String id);

	/**
	 * 
	 * @param userId
	 * @param id
	 * @return Null if not found
	 */
	Versioned<DiaryRecord> findById(int userId, String id);

	List<Versioned<DiaryRecord>> findByIdPrefix(int userId, String prefix);

	List<Versioned<DiaryRecord>> findChanged(int userId, Date since);

	List<Versioned<DiaryRecord>> findPeriod(int userId, Date startTime, Date endTime, boolean includeRemoved);

	/**
	 * Returns hash for specified ID prefix
	 * 
	 * @param userId
	 * @param prefix
	 *            Up to ID_PREFIX_SIZE chars long
	 * @return Null if hash not found
	 */
	String getHash(int userId, String prefix);

	void post(int userId, List<Versioned<DiaryRecord>> records);
}
