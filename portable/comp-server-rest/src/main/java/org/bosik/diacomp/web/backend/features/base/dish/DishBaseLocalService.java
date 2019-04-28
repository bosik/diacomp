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
package org.bosik.diacomp.web.backend.features.base.dish;

import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserDishItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

@Service
public class DishBaseLocalService
{
	private static final Parser<DishItem>     parser     = new ParserDishItem();
	private static final Serializer<DishItem> serializer = new SerializerAdapter<>(parser);

	@Autowired
	private CachedDishHashTree cachedHashTree;

	@Autowired
	private DishEntityRepository repository;

	private static Versioned<DishItem> convert(DishEntity e)
	{
		if (e == null)
		{
			return null;
		}

		final Versioned<DishItem> result = new Versioned<>();

		result.setId(e.getId());
		result.setTimeStamp(e.getTimeStamp());
		result.setHash(e.getHash());
		result.setVersion(e.getVersion());
		result.setDeleted(e.isDeleted());
		result.setData(serializer.read(e.getContent()));

		return result;
	}

	private static List<Versioned<DishItem>> convert(List<DishEntity> list)
	{
		return list.stream().map(DishBaseLocalService::convert).collect(toList());
	}

	private static void copyData(Versioned<DishItem> source, DishEntity destination)
	{
		destination.setTimeStamp(source.getTimeStamp());
		destination.setHash(source.getHash());
		destination.setVersion(source.getVersion());
		destination.setDeleted(source.isDeleted());
		destination.setContent(serializer.write(source.getData()));
		destination.setNameCache(source.getData().getName());
	}

	public void add(int userId, Versioned<DishItem> item) throws PersistenceException
	{
		save(userId, Collections.singletonList(item));
	}

	public int count(int userId)
	{
		return repository.countByUserId(userId);
	}

	public int count(int userId, String prefix)
	{
		return repository.countByUserIdAndIdStartingWith(userId, prefix);
	}

	public void delete(int userId, String id)
	{
		final DishEntity entity = repository.findByUserIdAndId(userId, id);

		if (entity == null)
		{
			throw new NotFoundException(id);
		}

		if (entity.isDeleted())
		{
			throw new AlreadyDeletedException(id);
		}

		final Versioned<DishItem> item = convert(entity);

		item.setDeleted(true);
		item.modified();

		save(userId, Collections.singletonList(item));
	}

	public List<Versioned<DishItem>> findAll(int userId, boolean includeRemoved)
	{
		if (includeRemoved)
		{
			return convert(repository.findByUserId(userId));
		}
		else
		{
			return convert(repository.findByUserIdAndDeletedIsFalse(userId));
		}
	}

	public List<Versioned<DishItem>> findAny(int userId, String filter)
	{
		// TODO: do we need this sorting?
		return convert(repository.findByUserIdAndDeletedIsFalseAndNameCacheContainingOrderByNameCache(userId, filter));
	}

	public Versioned<DishItem> findById(int userId, String id)
	{
		return convert(repository.findByUserIdAndId(userId, id));
	}

	public List<Versioned<DishItem>> findByIdPrefix(int userId, String prefix)
	{
		return convert(repository.findByUserIdAndIdStartingWith(userId, prefix));
	}

	public List<Versioned<DishItem>> findChanged(int userId, Date since)
	{
		return convert(repository.findByUserIdAndTimeStampAfter(userId, since));
	}

	public Versioned<DishItem> findOne(int userId, String exactName)
	{
		List<DishEntity> entities = repository.findByUserIdAndDeletedIsFalseAndNameCacheOrderByNameCache(userId, exactName);
		return entities.isEmpty() ? null : convert(entities.get(0));
	}

	/**
	 * @return Sorted map (ID, Hash) for all items
	 */
	private SortedMap<String, String> getDataHashes(int userId)
	{
		// TODO: check why Sorted is required
		// TODO: check performance
		Map<String, String> result = repository.findByUserId(userId).stream().collect(toMap(DishEntity::getId, DishEntity::getHash));
		return new TreeMap<>(result);
	}

	public MerkleTree getHashTree(int userId)
	{
		MerkleTree tree = cachedHashTree.get(userId);
		if (tree == null)
		{
			tree = HashUtils.buildMerkleTree(getDataHashes(userId));
			cachedHashTree.set(userId, tree);
		}

		return tree;
	}

	public void save(int userId, List<Versioned<DishItem>> items)
	{
		for (Versioned<DishItem> item : items)
		{
			if (item.getId() == null || item.getId().length() < ObjectService.ID_FULL_SIZE)
			{
				throw new IllegalArgumentException(
						String.format(Locale.US, "Invalid ID: %s, must be %d characters long", item.getId(), ObjectService.ID_FULL_SIZE));
			}

			DishEntity entity = repository.findByUserIdAndId(userId, item.getId());

			if (entity == null)
			{
				entity = new DishEntity();
				entity.setUserId(userId);
				entity.setId(item.getId());
			}

			copyData(item, entity);
			repository.save(entity);
			cachedHashTree.set(userId, null); // done in loop to reduce inconsistency window
		}
	}

	public String exportJson(int userId)
	{
		final StringBuilder s = new StringBuilder();
		s.append("[");

		// TODO: check performance
		repository.findByUserId(userId).forEach(entity ->
		{
			if (s.length() > 1)
			{
				s.append(",");
			}

			s.append("{");
			s.append("\"id\":\"").append(entity.getId()).append("\",");
			s.append("\"stamp\":\"").append(Utils.formatTimeUTC(entity.getTimeStamp())).append("\",");
			s.append("\"hash\":\"").append(entity.getHash()).append("\",");
			s.append("\"version\":").append(entity.getVersion()).append(",");
			s.append("\"deleted\":").append(entity.isDeleted()).append(",");
			s.append("\"data\":").append(entity.getContent());
			s.append("}");
		});

		s.append("]");
		return s.toString();
	}

	public String exportPlain(int userId)
	{
		final StringBuilder s = new StringBuilder();
		s.append("VERSION=1\n");

		// TODO: check performance
		repository.findByUserId(userId).forEach(entity ->
		{
			s.append(Utils.removeTabs(entity.getNameCache())).append('\t');
			s.append(entity.getId()).append('\t');
			s.append(Utils.formatTimeUTC(entity.getTimeStamp())).append('\t');
			s.append(entity.getHash()).append('\t');
			s.append(entity.getVersion()).append('\t');
			s.append(entity.isDeleted() ? "true" : "false").append('\t');
			s.append(entity.getContent()).append('\n');
		});

		return s.toString();
	}

	public void validate()
	{
		repository.findAll().forEach(entity ->
		{
			try
			{
				new JSONObject(entity.getContent());
			}
			catch (Exception e)
			{
				System.out.println(entity.getId() + "\t" + entity.getUserId() + "\t" + entity.getContent());
			}
		});
	}
}
