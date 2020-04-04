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
package org.bosik.diacomp.web.backend.features.base.food.user;

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.web.backend.common.UserDataService;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;
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
public class FoodUserLocalService implements UserDataService<FoodItem>
{
	@Autowired
	private CachedFoodUserHashTree cachedHashTree;

	@Autowired
	private FoodUserEntityRepository repository;

	public static Versioned<FoodItem> convert(FoodUserEntity e)
	{
		if (e == null)
		{
			return null;
		}

		final FoodItem food = new FoodItem();
		food.setName(e.getName());
		food.setRelProts(e.getProts());
		food.setRelFats(e.getFats());
		food.setRelCarbs(e.getCarbs());
		food.setRelValue(e.getValue());
		food.setFromTable(e.isFromTable());

		final Versioned<FoodItem> item = new Versioned<>();
		item.setId(e.getId().getId());
		item.setTimeStamp(e.getLastModified());
		item.setHash(e.getHash());
		item.setVersion(e.getVersion());
		item.setDeleted(e.isDeleted());
		item.setData(food);

		return item;
	}

	public static List<Versioned<FoodItem>> convert(List<FoodUserEntity> list)
	{
		return list.stream().map(FoodUserLocalService::convert).collect(toList());
	}

	private static void copyData(Versioned<FoodItem> source, FoodUserEntity destination)
	{
		destination.setName(source.getData().getName());
		destination.setProts(source.getData().getRelProts());
		destination.setFats(source.getData().getRelFats());
		destination.setCarbs(source.getData().getRelCarbs());
		destination.setValue(source.getData().getRelValue());
		destination.setFromTable(source.getData().getFromTable());

		destination.setLastModified(source.getTimeStamp());
		destination.setHash(source.getHash());
		destination.setVersion(source.getVersion());
		destination.setDeleted(source.isDeleted());
	}

	@Override
	public int count(int userId)
	{
		return repository.countByIdUserId(userId);
	}

	@Override
	public int count(int userId, String prefix)
	{
		if (prefix == null)
		{
			throw new IllegalArgumentException("ID prefix is null");
		}

		return repository.countByIdUserIdAndIdIdStartingWith(userId, prefix);
	}

	public void delete(int userId, String id)
	{
		final FoodUserEntity entity = repository.findByIdUserIdAndIdId(userId, id);

		if (entity == null)
		{
			throw new NotFoundException(id);
		}

		if (entity.isDeleted())
		{
			throw new AlreadyDeletedException(id);
		}

		Versioned<FoodItem> item = convert(entity);

		item.setDeleted(true);
		item.modified();

		save(userId, Collections.singletonList(item));
	}

	@Override
	public List<Versioned<FoodItem>> findAll(int userId, boolean includeRemoved)
	{
		if (includeRemoved)
		{
			return convert(repository.findByIdUserId(userId));
		}
		else
		{
			return convert(repository.findByIdUserIdAndDeletedIsFalse(userId));
		}
	}

	public List<Versioned<FoodItem>> findAny(int userId, String filter)
	{
		// TODO: do we need this sorting?
		return convert(repository.findByIdUserIdAndDeletedIsFalseAndNameContainingOrderByName(userId, filter));
	}

	@Override
	public Versioned<FoodItem> findById(int userId, String id)
	{
		return convert(repository.findByIdUserIdAndIdId(userId, id));
	}

	@Override
	public List<Versioned<FoodItem>> findByIdPrefix(int userId, String prefix)
	{
		return convert(repository.findByIdUserIdAndIdIdStartingWith(userId, prefix));
	}

	@Override
	public List<Versioned<FoodItem>> findChanged(int userId, Date time)
	{
		return convert(repository.findByIdUserIdAndLastModifiedIsGreaterThanEqual(userId, time));
	}

	public Versioned<FoodItem> findOne(int userId, String exactName)
	{
		List<FoodUserEntity> entities = repository.findByIdUserIdAndDeletedIsFalseAndNameOrderByName(userId, exactName);
		return entities.isEmpty() ? null : convert(entities.get(0));
	}

	/**
	 * @return Sorted map (ID, Hash) for all items
	 */
	public SortedMap<String, String> getDataHashes(int userId)
	{
		// TODO: check why Sorted is required
		// TODO: check performance
		Map<String, String> result = repository.findByIdUserId(userId).stream()
				.collect(toMap(e -> e.getId().getId(), FoodUserEntity::getHash));
		return new TreeMap<>(result);
	}

	@Override
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

	@Override
	public void save(int userId, List<Versioned<FoodItem>> items)
	{
		for (Versioned<FoodItem> item : items)
		{
			if (item.getId() == null || item.getId().length() < ObjectService.ID_FULL_SIZE)
			{
				throw new IllegalArgumentException(
						String.format(Locale.US, "Invalid ID: %s, must be %d characters long", item.getId(), ObjectService.ID_FULL_SIZE));
			}

			FoodUserEntity entity = repository.findByIdUserIdAndIdId(userId, item.getId());

			if (entity == null)
			{
				final FoodUserEntityPK id = new FoodUserEntityPK();
				id.setUserId(userId);
				id.setId(item.getId());

				entity = new FoodUserEntity();
				entity.setId(id);
			}

			copyData(item, entity);
			repository.save(entity);
			cachedHashTree.set(userId, null); // done in loop to reduce inconsistency window
		}
	}
}
