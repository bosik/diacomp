/*
 * MerkleSync - Data synchronization routine based on Merkle hash trees
 * Copyright (C) 2013 Nikita Bosik
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.bosik.merklesync;

import java.util.Date;
import java.util.List;

public interface DataSource<T>
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
	 * Returns item with the specified ID (no matter if deleted or not)
	 * 
	 * @param id
	 * @return Item if found, null otherwise
	 */
	Versioned<T> findById(String id);

	/**
	 * Returns list of records which id starts with specified prefix
	 * 
	 * @param prefix
	 *            Must be 0..ID_PREFIX_SIZE or ID_FULL_SIZE chars long
	 * @return
	 * @see #getDataHashes
	 */
	List<Versioned<T>> findByIdPrefix(String prefix);

	/**
	 * Returns list of records which were modified after the specified time (both removed or not)
	 * 
	 * @param since
	 * @return
	 */
	List<Versioned<T>> findChanged(Date since);

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
	 */
	void save(List<Versioned<T>> items);
}
