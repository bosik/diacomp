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
package org.bosik.diacomp.core.services;

import java.util.Map;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.merklesync.DataSource;

public interface ObjectService<T> extends DataSource<T>
{
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
	 * Returns children for specified node
	 * 
	 * @param prefix
	 *            Must be 0..ID_PREFIX_SIZE chars long
	 * @return Map (prefix + one_char, hash) if prefix is shorter than ID_PREFIX_SIZE; (id, hash) otherwise
	 * @throws CommonServiceException
	 */
	Map<String, String> getHashChildren(String prefix) throws CommonServiceException;
}
