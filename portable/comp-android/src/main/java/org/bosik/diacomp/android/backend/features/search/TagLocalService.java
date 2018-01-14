/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package org.bosik.diacomp.android.backend.features.search;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import org.bosik.diacomp.core.services.search.TagService;

public class TagLocalService implements TagService
{
	private static final Map<String, Integer>	cache	= new HashMap<>();
	private static final ReadWriteLock			lock	= new ReentrantReadWriteLock();

	@Override
	public Integer getTag(String id)
	{
		lock.readLock().lock();

		try
		{
			return cache.get(id);
		}
		finally
		{
			lock.readLock().unlock();
		}
	}

	@Override
	public void incTag(String id, int delta)
	{
		lock.writeLock().lock();

		try
		{
			Integer tag = cache.get(id);
			if (tag == null)
			{
				cache.put(id, delta);
			}
			else
			{
				cache.put(id, tag + delta);
			}
		}
		finally
		{
			lock.writeLock().unlock();
		}
	}

	@Override
	public void reset()
	{
		lock.writeLock().lock();

		try
		{
			cache.clear();
		}
		finally
		{
			lock.writeLock().unlock();
		}
	}
}