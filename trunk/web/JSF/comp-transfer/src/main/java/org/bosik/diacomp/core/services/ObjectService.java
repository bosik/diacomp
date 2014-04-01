package org.bosik.diacomp.core.services;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;

public interface ObjectService<T>
{
	/**
	 * Returns item with the specified GUID
	 *
	 * @param guid
	 * @return Item if found, null otherwise
	 * @throws CommonServiceException
	 */
	public abstract Versioned<T> getRecord(String guid) throws CommonServiceException;

	/**
	 * Returns list of records which were modified after the specified time (both removed or not)
	 *
	 * @param time
	 * @return
	 * @throws CommonServiceException
	 */
	public abstract List<Versioned<T>> getRecords(Date time) throws CommonServiceException;

	/**
	 * Persists records (creates if not exist, updates otherwise)
	 *
	 * @param records
	 * @throws CommonServiceException
	 */
	public abstract void postRecords(List<Versioned<T>> records) throws CommonServiceException;

}
