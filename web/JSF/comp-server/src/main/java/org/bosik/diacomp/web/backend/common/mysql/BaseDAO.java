package org.bosik.diacomp.web.backend.common.mysql;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.tech.Versioned;

public interface BaseDAO<T>
{
	/**
	 * Returns all items
	 *
	 * @param showRemoved
	 *            If removed items should be presented in the result
	 * @return
	 */
	List<Versioned<T>> findAll(int userId, boolean showRemoved);

	/**
	 * Searches for all items modified after specified time (both deleted and non-deleted)
	 *
	 * @param userId
	 * @param since
	 * @return
	 */
	List<Versioned<T>> findChanged(int userId, Date since);

	/**
	 * Searches the item with the specified GUID
	 *
	 * @param userId
	 * @param guids
	 * @return
	 */
	Versioned<T> findByGuid(int userId, String guid);

	/**
	 * Persists the specified items in the database
	 *
	 * @param userId
	 * @param items
	 */
	void save(int userId, List<Versioned<T>> items);

	/**
	 * Searches for items with the specified request filter
	 *
	 * @param userId
	 * @param filter
	 * @return
	 */
	List<Versioned<T>> findAny(int userId, String filter);
}