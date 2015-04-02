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
package org.bosik.diacomp.core.services.base;

import java.util.List;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.merklesync.Versioned;

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
