/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.web.backend.common;

import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;

import java.util.Date;
import java.util.List;

public interface UserDataService<T>
{
	/**
	 * @param userId User ID
	 * @return Count of all user entities (including removed ones)
	 */
	int count(int userId);

	/**
	 * @param userId User ID
	 * @param prefix ID prefix
	 * @return Count of all user entities with given prefix (including removed ones)
	 */
	int count(int userId, String prefix);

	/**
	 *
	 * @param userId User ID
	 * @param includeRemoved Should removed entities be included in the response
	 * @return All user's entities
	 */
	List<Versioned<T>> findAll(int userId, boolean includeRemoved);

	/**
	 * @param userId User ID
	 * @param id Entity ID
	 * @return Single entity if found (even if removed), {@code null} otherwise
	 */
	Versioned<T> findById(int userId, String id);

	/**
	 *
	 * @param userId User ID
	 * @param prefix ID prefix
	 * @return All user entities with given ID prefix (including removed ones)
	 */
	List<Versioned<T>> findByIdPrefix(int userId, String prefix);

	/**
	 *
	 * @param userId User ID
	 * @param time Time
	 * @return All user entries modified after the time given (including removed ones)
	 */
	List<Versioned<T>> findChanged(int userId, Date time);

	/**
	 * @param userId User ID
	 * @return Data merkle tree for comparison and sync
	 */
	MerkleTree getHashTree(int userId);

	/**
	 * Saved given entities
	 * @param userId User ID
	 * @param items Items to save
	 */
	void save(int userId, List<Versioned<T>> items);
}
