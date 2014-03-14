package org.bosik.diacomp.core.services.diary;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;

/**
 * Diary records service
 *
 * @author Bosik
 */
public interface DiaryService
{
	/**
	 * Returns records with the specified GUID
	 *
	 * @param guid
	 * @return Item if found, null otherwise
	 * @throws CommonServiceException
	 */
	public Versioned<DiaryRecord> getRecord(String guid) throws CommonServiceException;

	/**
	 * Returns list of records which were modified after the specified time (both removed or not)
	 *
	 * @param time
	 * @return
	 * @throws CommonServiceException
	 */
	public List<Versioned<DiaryRecord>> getRecords(Date time) throws CommonServiceException;

	/**
	 * Returns list of non-deleted records for the specified time interval
	 *
	 * @param fromTime
	 * @param toTime
	 * @param includeRemoved
	 * @return
	 * @throws CommonServiceException
	 */
	public List<Versioned<DiaryRecord>> getRecords(Date fromTime, Date toTime, boolean includeRemoved)
			throws CommonServiceException;

	/**
	 * Persists records (create if not exist, update otherwise)
	 *
	 * @param records
	 * @throws CommonServiceException
	 */
	public void postRecords(List<Versioned<DiaryRecord>> records) throws CommonServiceException;
}
