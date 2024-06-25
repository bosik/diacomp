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

import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.merklesync.Versioned;

import java.util.List;

public interface BaseService<Item> extends ObjectService<Item>
{
	/**
	 * Returns all items
	 *
	 * @param includeDeleted Should the deleted entries be included
	 * @return List of all items, may be empty; never null
	 */
	List<Versioned<Item>> findAll(boolean includeDeleted);

	/**
	 * Searches for non-deleted items with a name containing specified substring (case insensitive).
	 *
	 * @param filter Substring to search
	 * @return List of items found, may be empty; never null
	 */
	List<Versioned<Item>> findAny(String filter);

	/**
	 * Searches for a non-deleted item with exact name
	 *
	 * @param exactName Item name
	 * @return Item if found, null otherwise
	 */
	Versioned<Item> findOne(String exactName);
}
