package org.bosik.diacomp.core.services;

import java.util.List;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;

public interface BaseService<Item> extends ObjectService<Item>
{
	/**
	 * Adds item
	 *
	 * @param item
	 * @throws DuplicateException
	 *             If item already presented
	 * @throws PersistenceException
	 *             Common inserting failure
	 */
	// TODO: move to ObjectService
	void add(Versioned<Item> item) throws DuplicateException, PersistenceException;

	/**
	 * Marks item with specified ID as deleted
	 *
	 * @param id
	 * @throws NotFoundException
	 *             If no item with such ID found
	 * @throws AlreadyDeletedException
	 *             If item is already deleted
	 */
	// TODO: move to ObjectService
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
}
