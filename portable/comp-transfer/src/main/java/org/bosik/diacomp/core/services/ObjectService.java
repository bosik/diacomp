package org.bosik.diacomp.core.services;

import java.util.Date;
import java.util.List;
import java.util.Map;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.diacomp.core.services.sync.MerkleTree;

public interface ObjectService<T>
{
	/**
	 * Size of standard ID
	 */
	static final int	ID_FULL_SIZE	= 32;
	/**
	 * Size of ID prefix used in hash trees
	 */
	static final int	ID_PREFIX_SIZE	= 4;
	/**
	 * Max number of items returned
	 */
	static final int	MAX_ITEMS_COUNT	= 500;

	/**
	 * Calculates number of objects with specified ID prefix
	 * 
	 * @param prefix
	 * @return
	 */
	int count(String prefix);

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
	 * Returns item with the specified ID (no matter if deleted or not)
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
	 *            Must be 0..ID_PREFIX_SIZE or ID_FULL_SIZE chars long
	 * @return
	 * @throws TooManyItemsException
	 *             If there are more than MAX_ITEMS_COUNT items to return
	 * @see #getDataHashes
	 */
	List<Versioned<T>> findByIdPrefix(String prefix) throws TooManyItemsException;

	/**
	 * Returns list of records which were modified after the specified time (both removed or not)
	 * 
	 * @param since
	 * @return
	 * @throws CommonServiceException
	 */
	List<Versioned<T>> findChanged(Date since) throws CommonServiceException;

	/**
	 * 
	 * @param prefix
	 *            Must be 0..ID_PREFIX_SIZE chars long
	 * @return Hash for specified ID prefix, or null if hash not found
	 * @throws CommonServiceException
	 */
	String getHash(String prefix) throws CommonServiceException;

	/**
	 * Returns children for specified node
	 * 
	 * @param prefix
	 *            Must be 0..ID_PREFIX_SIZE chars long
	 * @return Map (prefix + one_char, hash) if prefix is shorter than ID_PREFIX_SIZE; (id, hash) otherwise
	 * @throws CommonServiceException
	 */
	Map<String, String> getHashChildren(String prefix) throws CommonServiceException;

	/**
	 * Returns hash tree
	 * 
	 * @return
	 */
	MerkleTree getHashTree();

	/**
	 * Persists items (creates if not exist, updates otherwise)
	 * 
	 * @param items
	 * @throws CommonServiceException
	 */
	void save(List<Versioned<T>> items) throws CommonServiceException;
}
