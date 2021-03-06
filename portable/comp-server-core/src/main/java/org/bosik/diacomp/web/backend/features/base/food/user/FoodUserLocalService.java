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

import org.apache.commons.lang3.StringUtils;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.web.backend.common.UserDataService;
import org.bosik.diacomp.web.backend.features.base.food.combo.FoodEntity;
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

	public static Versioned<FoodItem> convert(FoodEntity e)
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
		item.setId(e.getId());
		item.setTimeStamp(e.getLastModified());
		item.setHash(e.getHash());
		item.setVersion(e.getVersion());
		item.setDeleted(e.isDeleted());
		item.setData(food);

		return item;
	}

	public static List<Versioned<FoodItem>> convert(List<? extends FoodEntity> list)
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
		return repository.countByKeyUserId(userId);
	}

	@Override
	public int count(int userId, String prefix)
	{
		if (prefix == null)
		{
			throw new IllegalArgumentException("ID prefix is null");
		}

		return repository.countByKeyUserIdAndKeyIdStartingWith(userId, prefix);
	}

	public void delete(int userId, String id)
	{
		final FoodUserEntity entity = repository.findByKeyUserIdAndKeyId(userId, id);

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
			return convert(repository.findByKeyUserId(userId));
		}
		else
		{
			return convert(repository.findByKeyUserIdAndDeletedIsFalse(userId));
		}
	}

	public List<Versioned<FoodItem>> findAny(int userId, String filter)
	{
		// TODO: do we need this sorting?
		return convert(repository.findByKeyUserIdAndDeletedIsFalseAndNameContaining(userId, filter));
	}

	@Override
	public Versioned<FoodItem> findById(int userId, String id)
	{
		return convert(repository.findByKeyUserIdAndKeyId(userId, id));
	}

	@Override
	public List<Versioned<FoodItem>> findByIdPrefix(int userId, String prefix)
	{
		return convert(repository.findByKeyUserIdAndKeyIdStartingWith(userId, prefix));
	}

	@Override
	public List<Versioned<FoodItem>> findChanged(int userId, Date time)
	{
		return convert(repository.findByKeyUserIdAndLastModifiedIsGreaterThanEqual(userId, time));
	}

	public Versioned<FoodItem> findOne(int userId, String exactName)
	{
		List<FoodUserEntity> entities = repository.findByKeyUserIdAndDeletedIsFalseAndName(userId, exactName);
		return entities.isEmpty() ? null : convert(entities.get(0));
	}

	/**
	 * @return Sorted map (ID, Hash) for all items
	 */
	public SortedMap<String, String> getDataHashes(int userId)
	{
		// TODO: check why Sorted is required
		// TODO: check performance
		Map<String, String> result = repository.findByKeyUserId(userId).stream()
				.collect(toMap(e -> e.getKey().getId(), FoodUserEntity::getHash));
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
			if (item.getId() == null || item.getId().length() != ObjectService.ID_FULL_SIZE)
			{
				throw new IllegalArgumentException(
						String.format(Locale.US, "Invalid ID: %s, must be %d characters long", item.getId(), ObjectService.ID_FULL_SIZE));
			}

			validate(item.getData());

			FoodUserEntity entity = repository.findByKeyUserIdAndKeyId(userId, item.getId());

			if (entity == null)
			{
				final FoodUserEntityPK id = new FoodUserEntityPK();
				id.setUserId(userId);
				id.setId(item.getId());

				entity = new FoodUserEntity();
				entity.setKey(id);
			}

			copyData(item, entity);
			repository.save(entity);
			cachedHashTree.set(userId, null); // done in loop to reduce inconsistency window
		}
	}

	private static void validate(FoodItem data)
	{
		if (data.getName() == null)
		{
			throw new IllegalArgumentException("Name can't be null");
		}

		if (data.getName().length() > FoodUserEntity.MAX_SIZE_NAME)
		{
			throw new IllegalArgumentException("Name too long, max " + FoodUserEntity.MAX_SIZE_NAME + " chars allowed: " +
					StringUtils.abbreviate(data.getName(), 2 * FoodUserEntity.MAX_SIZE_NAME)
			);
		}
	}
}
