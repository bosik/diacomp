package org.bosik.diacomp.core.services;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;

public interface ObjectService<T>
{
	/**
	 * Returns item with the specified GUID (no matter if deleted or not)
	 * 
	 * @param guid
	 * @return Item if found, null otherwise
	 * @throws CommonServiceException
	 */
	Versioned<T> findById(String guid) throws CommonServiceException;

	/**
	 * Returns list of records which were modified after the specified time (both removed or not)
	 * 
	 * @param since
	 * @return
	 * @throws CommonServiceException
	 */
	List<Versioned<T>> findChanged(Date since) throws CommonServiceException;

	/**
	 * Persists items (creates if not exist, updates otherwise)
	 * 
	 * @param items
	 * @throws CommonServiceException
	 */
	// TODO: retrieve version and increment it here
	// TODO: set timestamp to current time
	void save(List<Versioned<T>> items) throws CommonServiceException;
}
