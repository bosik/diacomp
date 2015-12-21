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
	 * Size of hash (chars)
	 */
	int	HASH_SIZE		= 32;
	/**
	 * Size of ID prefix used in hash trees (chars)
	 */
	int	ID_PREFIX_SIZE	= 4;

	/**
	 * @param prefix
	 *            Must be 0..ID_PREFIX_SIZE chars long
	 * @return Number of items having ID started with prefix
	 */
	int count(String prefix);

	/**
	 * Returns item with the specified ID (no matter if deleted or not)
	 * 
	 * @param id
	 * @return Item if found, <code>null</code> otherwise
	 */
	Versioned<T> findById(String id);

	/**
	 * @param prefix
	 *            Must be 0..ID_PREFIX_SIZE chars long
	 * @return List of items having ID started with the specified prefix
	 */
	List<Versioned<T>> findByIdPrefix(String prefix);

	/**
	 * @param since
	 * @return List of items modified after the <code>since</code> time (both deleted or not)
	 */
	List<Versioned<T>> findChanged(Date since);

	/**
	 * @return Hash tree
	 */
	MerkleTree getHashTree();

	/**
	 * Persists items (creates if not exist, updates otherwise)
	 * 
	 * @param items
	 */
	void save(List<Versioned<T>> items);
}
