package org.bosik.diacomp.core.services;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;

public interface ObjectService<T>
{
	static final int	ID_PREFIX_SIZE	= 4;

	/**
	 * Marks item with specified ID as deleted
	 * 
	 * @param id
	 * @throws NotFoundException
	 *             If no item with such ID found
	 * @throws AlreadyDeletedException
	 *             If item is already deleted
	 */
	void delete(String id) throws NotFoundException, AlreadyDeletedException;

	/**
	 * Returns item with the specified GUID (no matter if deleted or not)
	 * 
	 * @param id
	 * @return Item if found, null otherwise
	 * @throws CommonServiceException
	 */
	Versioned<T> findById(String id) throws CommonServiceException;

	/**
	 * Returns list of records which id starts with specified prefix
	 * 
	 * @param prefix
	 *            Prefix, must be ID_PREFIX_SIZE chars long
	 * @return
	 * @throws CommonServiceException
	 */
	List<Versioned<T>> findByIdPrefix(String prefix) throws CommonServiceException;

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
	void save(List<Versioned<T>> items) throws CommonServiceException;
}
