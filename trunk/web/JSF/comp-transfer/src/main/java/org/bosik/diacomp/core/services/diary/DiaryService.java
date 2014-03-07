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
	 * Returns list of records with the specified GUIDs
	 * 
	 * @param guids
	 * @return
	 * @throws CommonServiceException
	 *             If any GUID doesn't found
	 */
	public List<Versioned<DiaryRecord>> getRecords(List<String> guids) throws CommonServiceException;

	/**
	 * Returns list of records which were modified after the specified time
	 * 
	 * @param time
	 * @param includeRemoved
	 * @return
	 * @throws CommonServiceException
	 */
	public List<Versioned<DiaryRecord>> getRecords(Date time, boolean includeRemoved) throws CommonServiceException;

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
