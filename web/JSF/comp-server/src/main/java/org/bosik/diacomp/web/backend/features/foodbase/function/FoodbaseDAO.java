package org.bosik.diacomp.web.backend.features.foodbase.function;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;

public interface FoodbaseDAO
{
	/**
	 * Returns all items
	 *
	 * @param showRemoved
	 *            If removed items should be presented in the result
	 * @return
	 */
	List<Versioned<FoodItem>> findAll(int userId, boolean showRemoved);

	/**
	 * Searches for all items modified after specified time (both deleted and non-deleted)
	 *
	 * @param userId
	 * @param since
	 * @return
	 */
	List<Versioned<FoodItem>> findChanged(int userId, Date since);

	/**
	 * Searches the item with the specified GUID
	 *
	 * @param userId
	 * @param guids
	 * @return
	 */
	Versioned<FoodItem> findByGuid(int userId, String guid);

	/**
	 * Persists the specified items in the database (i.e. updates if item exists, inserts otherwise)
	 * 
	 * @param userId
	 * @param items
	 */
	void save(int userId, List<Versioned<FoodItem>> items);

	/**
	 * Searches for items with the specified request filter
	 *
	 * @param userId
	 * @param filter
	 * @return
	 */
	List<Versioned<FoodItem>> findAny(int userId, String filter);
}