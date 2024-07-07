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
import org.bosik.diacomp.core.utils.CollectionUtils;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;

import static org.bosik.diacomp.core.utils.CollectionUtils.filter;

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

	private <X> Versioned<X> clone(Versioned<X> item)
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

	private <X> List<Versioned<X>> clone(Collection<Versioned<X>> items)
	{
		List<Versioned<X>> cloned = new ArrayList<>();

		for (Versioned<X> item : items)
		{
			cloned.add(clone(item));
		}

		return cloned;
	}

	// INTERNALS

	protected void rebuildCache()
	{
		memoryCache = index(loadAllFromDb());
	}

	private void insert(Versioned<T> item)
	{
		memoryCache.put(item.getId(), clone(item));
		insertDb(item);
	}

	private void update(Versioned<T> item)
	{
		memoryCache.put(item.getId(), clone(item));
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
		if (includeDeleted)
		{
			return clone(memoryCache.values());
		}
		else
		{
			return clone(filter(memoryCache.values(), new CollectionUtils.Predicate<Versioned<T>>()
			{
				@Override
				public boolean test(Versioned<T> item)
				{
					return !item.isDeleted();
				}
			}));
		}
	}

	@Override
	public List<Versioned<T>> findAny(final String filter)
	{
		if (filter != null)
		{
			final List<String> tokens = Utils.parseTokens(filter);
			return clone(filter(memoryCache.values(), new CollectionUtils.Predicate<Versioned<T>>()
			{
				@Override
				public boolean test(Versioned<T> item)
				{
					return Utils.matchesTokens(item.getData().getName(), tokens) &&  !item.isDeleted();
				}
			}));
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

		List<Versioned<T>> items = filter(memoryCache.values(), new CollectionUtils.Predicate<Versioned<T>>()
		{
			@Override
			public boolean test(Versioned<T> item)
			{
				return !item.isDeleted() && item.getData().getName().trim().toLowerCase(Locale.US).equals(exactNameTrim);
			}
		});

		return !items.isEmpty() ? clone(items.get(0)) : null;
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
		if (prefix == null)
		{
			return clone(memoryCache.values());
		}
		else
		{
			return clone(filter(memoryCache.values(), new CollectionUtils.Predicate<Versioned<T>>()
			{
				@Override
				public boolean test(Versioned<T> item)
				{
					return item.getId().startsWith(prefix);
				}
			}));
		}
	}

	@Override
	public List<Versioned<T>> findChanged(final Date since)
	{
		if (since == null)
		{
			return clone(memoryCache.values());
		}
		else
		{
			return clone(filter(memoryCache.values(), new CollectionUtils.Predicate<Versioned<T>>()
			{
				@Override
				public boolean test(Versioned<T> item)
				{
					return item.getTimeStamp().after(since);
				}
			}));
		}
	}

	@Override
	public MerkleTree getHashTree()
	{
		SortedMap<String, String> hashes = new TreeMap<>();

		for (Versioned<T> item : memoryCache.values())
		{
			hashes.put(item.getId(), item.getHash());
		}

		return HashUtils.buildMerkleTree(hashes);
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
