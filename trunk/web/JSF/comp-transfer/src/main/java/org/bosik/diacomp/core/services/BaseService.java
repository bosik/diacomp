package org.bosik.diacomp.core.services;

import java.util.List;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;

public interface BaseService<Item>
{
	/**
	 * Adds item to the list
	 *
	 * @param item
	 * @return ID of created item
	 * @throws PersistenceException
	 *             If storing failed
	 */
	String add(Versioned<Item> item) throws PersistenceException;

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
	 * Returns all items
	 *
	 * @param includeRemoved
	 * @return
	 */
	List<Versioned<Item>> findAll(boolean includeRemoved);

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
	 * Searches for items with specified ID (both deleted or not)
	 *
	 * @param guid
	 * @return
	 */
	Versioned<Item> findById(String guid);

	/**
	 * Updates single non-deleted item. Note: updating deleted item result in exception.
	 *
	 * @param item
	 * @throws NotFoundException
	 *             If no non-deleted item with such ID found
	 * @throws PersistenceException
	 *             If storing failed
	 */

	// TODO: retrieve version and increment it here
	// TODO: set timestamp to current time
	void save(List<Versioned<Item>> items) throws NotFoundException, PersistenceException;
}
