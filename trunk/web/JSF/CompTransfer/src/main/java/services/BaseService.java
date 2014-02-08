package services;

import java.util.List;
import org.bosik.diacomp.persistence.common.Versioned;
import org.bosik.diacomp.persistence.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.persistence.exceptions.ItemNotFoundException;
import org.bosik.diacomp.persistence.exceptions.StoreException;

public interface BaseService<Item>
{
	/**
	 * Adds item to the list
	 * 
	 * @param item
	 * @return ID of created item
	 * @throws StoreException
	 *             If storing failed
	 */
	String add(Versioned<Item> item) throws StoreException;

	/**
	 * Marks item with specified ID as deleted
	 * 
	 * @param id
	 * @throws ItemNotFoundException
	 *             If no item with such ID found
	 * @throws AlreadyDeletedException
	 *             If item is already deleted
	 */
	void delete(String id) throws ItemNotFoundException, AlreadyDeletedException;

	/**
	 * Returns all non-deleted items
	 * 
	 * @return
	 */
	List<Versioned<Item>> findAll();

	/**
	 * Searches for non-deleted item with name containing specified string (case insensitive).
	 * 
	 * @param filter
	 * @return Item if found, null otherwise
	 */
	List<Versioned<Item>> findAny(String filter);

	/**
	 * Searches for non-deleted item with exact name
	 * 
	 * @param exactName
	 * @return Item if found, null otherwise
	 */
	Versioned<Item> findOne(String exactName);

	/**
	 * Searches for non-deleted item with specified ID
	 * 
	 * @param id
	 * @return
	 */
	Versioned<Item> findById(String id);

	/**
	 * Searches for all items (both deleted or not)
	 * 
	 * @return
	 */
	List<Versioned<Item>> findSysAll();

	/**
	 * Searches for item (both deleted or not) with specified ID
	 * 
	 * @param id
	 * @return
	 */
	Versioned<Item> findSysById(String id);

	/**
	 * Updates single non-deleted item. Note: updating deleted item result in exception.
	 * 
	 * @param item
	 * @throws ItemNotFoundException
	 *             If no non-deleted item with such ID found
	 * @throws StoreException
	 *             If storing failed
	 */

	// TODO: retrieve version and increment it here
	// TODO: set timestamp to current time
	void update(Versioned<Item> item) throws ItemNotFoundException, StoreException;
}
