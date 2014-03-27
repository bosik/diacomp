package org.bosik.diacomp.web.backend.features.dishbase.function;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;

public interface DishbaseDAO
{
	/**
	 * Returns all items
	 *
	 * @param showRemoved
	 *            If removed items should be presented in the result
	 * @return
	 */
	List<Versioned<DishItem>> findAll(int userId, boolean showRemoved);

	/**
	 * Searches for all items modified after specified time (both deleted and non-deleted)
	 *
	 * @param userId
	 * @param since
	 * @return
	 */
	List<Versioned<DishItem>> findChanged(int userId, Date since);

	/**
	 * Searches the item with the specified GUID
	 *
	 * @param userId
	 * @param guids
	 * @return
	 */
	Versioned<DishItem> findByGuid(int userId, String guid);

	/**
	 * Persists the specified items in the database
	 *
	 * @param userId
	 * @param items
	 */
	void post(int userId, List<Versioned<DishItem>> items);

	/**
	 * Searches for items with the specified request filter
	 *
	 * @param userId
	 * @param filter
	 * @return
	 */
	List<Versioned<DishItem>> findAny(int userId, String filter);
}
