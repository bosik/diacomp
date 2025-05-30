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
package org.bosik.diacomp.android.backend.common;

import org.bosik.diacomp.core.entities.business.interfaces.Named;
import org.bosik.diacomp.core.services.base.BaseService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.Versioned;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

public abstract class CachedBaseService<T extends Named> implements BaseService<T>
{
	// caching
	// NOTE: this assumes DB can't be changed outside of the app
	private volatile Map<String, Versioned<T>> memoryCache;

	// ROUTINES

	private static <T> Map<String, Versioned<T>> index(Iterable<Versioned<T>> items)
	{
		Map<String, Versioned<T>> map = new ConcurrentHashMap<>();

		for (Versioned<T> item : items)
		{
			map.put(item.getId(), item);
		}

		return map;
	}

	private static <X> Versioned<X> clone(Versioned<X> item)
	{
		if (item != null)
		{
			Versioned<X> clone = new Versioned<>(item);

			// Turning serialization on slows down opening meal/dish editors significantly
			// clone.setData(serializer.read(serializer.write(item.getData())));

			return clone;
		}
		else
		{
			return null;
		}
	}

	// INTERNALS

	protected void rebuildCache()
	{
		memoryCache = index(loadAllFromDb());
	}

	private synchronized void putEntry(Versioned<T> item)
	{
		memoryCache.put(item.getId(), clone(item));
	}

	private void insert(Versioned<T> item)
	{
		putEntry(item);
		insertDb(item);
	}

	private void update(Versioned<T> item)
	{
		putEntry(item);
		updateDb(item);
	}

	// SEMI-PUBLIC API

	public boolean recordExists(String id)
	{
		return memoryCache.containsKey(id);
	}

	// PUBLIC API

	@Override
	public void add(Versioned<T> item) throws DuplicateException
	{
		if (!recordExists(item.getId()))
		{
			insert(item);
		}
		else
		{
			throw new DuplicateException(item.getId());
		}
	}

	@Override
	public int count(String prefix)
	{
		if (prefix == null)
		{
			throw new IllegalArgumentException("ID prefix is null");
		}

		if (prefix.isEmpty())
		{
			return memoryCache.size();
		}
		else
		{
			int count = 0;

			for (String id : memoryCache.keySet())
			{
				if (id.startsWith(prefix))
				{
					count++;
				}
			}

			return count;
		}
	}

	@Override
	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	{
		Versioned<T> item = findById(id);

		if (item == null)
		{
			throw new NotFoundException(id);
		}

		if (item.isDeleted())
		{
			throw new AlreadyDeletedException(id);
		}

		item.setDeleted(true);
		item.modified();
		update(item);
	}

	@Override
	public List<Versioned<T>> findAll(final boolean includeDeleted)
	{
		return memoryCache.values().stream()
				.filter(item -> includeDeleted || !item.isDeleted())
				.map(CachedBaseService::clone)
				.collect(Collectors.toList());
	}

	@Override
	public List<Versioned<T>> findAny(final String filter)
	{
		if (filter != null)
		{
			final List<String> tokens = Utils.parseTokens(filter);
			return memoryCache.values().stream()
					.filter(item -> Utils.matchesTokens(item.getData().getName(), tokens) && !item.isDeleted())
					.map(CachedBaseService::clone)
					.collect(Collectors.toList());
		}
		else
		{
			return findAll(false);
		}
	}

	@Override
	public Versioned<T> findOne(String exactName)
	{
		final String exactNameTrim = exactName.trim().toLowerCase(Locale.US);

		return memoryCache.values().stream()
				.filter(item -> !item.isDeleted() && item.getData().getName().trim().toLowerCase(Locale.US).equals(exactNameTrim))
				.findFirst()
				.map(CachedBaseService::clone)
				.orElse(null);
	}

	@Override
	public Versioned<T> findById(final String id)
	{
		if (id == null)
		{
			throw new IllegalArgumentException("id is null");
		}

		return clone(memoryCache.get(id));
	}

	@Override
	public List<Versioned<T>> findByIdPrefix(final String prefix) throws CommonServiceException
	{
		return memoryCache.values().stream()
				.filter(item -> prefix == null || item.getId().startsWith(prefix))
				.map(CachedBaseService::clone)
				.collect(Collectors.toList());
	}

	@Override
	public List<Versioned<T>> findChanged(final Date since)
	{
		return memoryCache.values().stream()
				.filter(item -> since == null || item.getTimeStamp().after(since))
				.map(CachedBaseService::clone)
				.collect(Collectors.toList());
	}

	private List<String> getDataHashes(String prefix)
	{
		return memoryCache.values()
				.stream()
				.filter(e -> e.getId().startsWith(prefix))
				.map(Versioned::getHash)
				.collect(Collectors.toList());
	}

	@Override
	public String getHash(String prefix)
	{
		return HashUtils.sum(getDataHashes(prefix));
	}

	@Override
	public Map<String, String> getHashChildren(String prefix)
	{
		final Map<String, String> map = new HashMap<>();

		for (int i = 0; i < 16; i++)
		{
			final String key = prefix + HashUtils.byteToChar(i);
			map.put(key, HashUtils.sum(getDataHashes(key)));
		}

		return map;
	}

	@Override
	public void save(List<Versioned<T>> items) throws PersistenceException
	{
		for (Versioned<T> item : items)
		{
			if (recordExists(item.getId()))
			{
				update(item);
			}
			else
			{
				insert(item);
			}
		}
	}

	// ABSTRACTS

	protected abstract List<Versioned<T>> loadAllFromDb();

	protected abstract void insertDb(Versioned<T> item);

	protected abstract void updateDb(Versioned<T> item);
}
