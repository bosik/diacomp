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
	int count(int userId);

	int count(int userId, String prefix);

	List<Versioned<T>> findAll(int userId, boolean includeRemoved);

	Versioned<T> findById(int userId, String id);

	List<Versioned<T>> findByIdPrefix(int userId, String prefix);

	List<Versioned<T>> findChanged(int userId, Date since);

	MerkleTree getHashTree(int userId);

	void save(int userId, List<Versioned<T>> items);
}
