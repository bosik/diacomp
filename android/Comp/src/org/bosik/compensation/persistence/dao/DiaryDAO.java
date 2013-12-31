package org.bosik.compensation.persistence.dao;

import java.util.Date;
import java.util.List;
import org.bosik.compensation.bo.diary.DiaryRecord;
import org.bosik.compensation.persistence.common.Versioned;
import org.bosik.compensation.persistence.exceptions.CommonDAOException;

/**
 * Diary records DAO
 * 
 * @author Bosik
 */
public interface DiaryDAO
{
	/**
	 * Returns list of records with the specified GUIDs
	 * 
	 * @param guids
	 * @return
	 * @throws CommonDAOException
	 *             If any GUID doesn't found
	 */
	public List<Versioned<DiaryRecord>> getRecords(List<String> guids) throws CommonDAOException;

	/**
	 * Returns list of records which were modified after the specified time
	 * 
	 * @param time
	 * 
	 * @return
	 */
	public List<Versioned<DiaryRecord>> getRecords(Date time) throws CommonDAOException;

	// FIXME: no DELETED handling implemented

	/**
	 * Returns list of non-deleted records for the specified time interval
	 * 
	 * @param fromDate
	 * @param toDate
	 * @return
	 * @throws CommonDAOException
	 */
	public List<Versioned<DiaryRecord>> getRecords(Date fromDate, Date toDate) throws CommonDAOException;

	/**
	 * Persists records (create if not exist, update otherwise)
	 * 
	 * @param records
	 * @throws CommonDAOException
	 */
	public void postRecords(List<Versioned<DiaryRecord>> records) throws CommonDAOException;
}
