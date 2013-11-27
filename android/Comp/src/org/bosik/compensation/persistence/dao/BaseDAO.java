package org.bosik.compensation.persistence.dao;

import java.util.List;

public interface BaseDAO<Item>
{
	public static class ItemNotFoundException extends Exception
	{
		private static final long	serialVersionUID	= 1L;

		public ItemNotFoundException(String id)
		{
			super(String.format("Item %s not found", id));
		}
	}

	public static class DuplicateException extends Exception
	{
		private static final long	serialVersionUID	= 1L;

		public DuplicateException(String id)
		{
			super(String.format("Item %s already exists", id));
		}
	}

	/**
	 * Adds item to the list
	 * 
	 * @param item
	 * @return ID of created item
	 * @throws DuplicateException
	 *             If item with such ID already presented
	 */
	String add(Item item) throws DuplicateException;

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
	List<Item> findAll();

	/**
	 * Searched for any item which has filter as substring in it's name (case insensitive)
	 * 
	 * @param filter
	 * @return
	 */
	List<Item> findAny(String filter);

	/**
	 * Searches for item with specified name, returns null if not found
	 * 
	 * @param exactName
	 * @return
	 */
	Item findOne(String exactName);

	/**
	 * Replaces all items by specified one and sets specified list version; this is useful in sync
	 * procedures
	 * 
	 * @param newList
	 * @param newVersion
	 */
	void replaceAll(List<Item> newList, int newVersion);

	/**
	 * Updates single item
	 * 
	 * @param item
	 * @throws ItemNotFoundException
	 *             If no item with such ID found
	 */
	void update(Item item) throws ItemNotFoundException;

	/**
	 * Returns DAO's list version
	 * 
	 * @return
	 */
	int getVersion();
}
