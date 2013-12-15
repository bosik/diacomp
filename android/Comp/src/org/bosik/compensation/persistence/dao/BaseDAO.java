package org.bosik.compensation.persistence.dao;

import java.util.List;
import org.bosik.compensation.persistence.common.Versioned;
import org.bosik.compensation.persistence.exceptions.ItemNotFoundException;
import org.bosik.compensation.persistence.exceptions.StoreException;

public interface BaseDAO<Item>
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
	 * Removes item with specified ID
	 * 
	 * @param id
	 * @throws ItemNotFoundException
	 *             If no item with such ID found
	 */
	void delete(String id) throws ItemNotFoundException;

	/**
	 * Returns all items
	 * 
	 * @return
	 */
	List<Versioned<Item>> findAll();

	/**
	 * Searched for any item which has filter as substring in it's name (case insensitive)
	 * 
	 * @param filter
	 * @return
	 */
	List<Versioned<Item>> findAny(String filter);

	/**
	 * Searches for item with specified name, returns null if not found
	 * 
	 * @param exactName
	 * @return
	 */
	Versioned<Item> findOne(String exactName);

	/**
	 * Searches for item with specified ID, returns null if not found
	 * 
	 * @param id
	 * @return
	 */
	Versioned<Item> findById(String id);

	/**
	 * Replaces all items by specified one and sets specified list version; this is useful in sync
	 * procedures
	 * 
	 * @param newList
	 * @param newVersion
	 * @throws StoreException
	 *             If storing failed
	 */
	// @Deprecated
	// void replaceAll(List<Item> newList, int newVersion) throws StoreException;

	/**
	 * Updates single item
	 * 
	 * @param item
	 * @throws ItemNotFoundException
	 *             If no item with such ID found
	 * @throws StoreException
	 *             If storing failed
	 */
	void update(Versioned<Item> item) throws ItemNotFoundException, StoreException;

	/**
	 * Returns DAO's list version
	 * 
	 * @return
	 */
	// @Deprecated
	// int getVersion();
}
