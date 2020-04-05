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
package org.bosik.diacomp.web.backend.features.base.food.common;

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.services.base.food.FoodCommonService;
import org.bosik.merklesync.Versioned;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

@Service
public class FoodCommonLocalService implements FoodCommonService
{
	@Autowired
	private FoodCommonEntityRepository repository;

	private static Versioned<FoodItem> convert(FoodCommonEntity e)
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

	public static List<Versioned<FoodItem>> convert(List<FoodCommonEntity> list)
	{
		return list.stream().map(FoodCommonLocalService::convert).collect(toList());
	}

	@Override
	public List<Versioned<FoodItem>> findAll()
	{
		return convert(repository.findAll());
	}

	public List<Versioned<FoodItem>> findAll(boolean includeRemoved)
	{
		if (includeRemoved)
		{
			return findAll();
		}
		else
		{
			return convert(repository.findByDeletedIsFalse());
		}
	}

	@Override
	public List<Versioned<FoodItem>> findChanged(Date lastModified)
	{
		return convert(repository.findByLastModifiedIsGreaterThanEqual(lastModified));
	}

	public List<Versioned<FoodItem>> findAny(String filter)
	{
		return convert(repository.findByDeletedIsFalseAndNameContains(filter));
	}

	public Versioned<FoodItem> findById(String id)
	{
		return convert(repository.findById(id).orElse(null));
	}

	public List<Versioned<FoodItem>> findByIdPrefix(String prefix)
	{
		return convert(repository.findByIdStartingWith(prefix));
	}

	public Versioned<FoodItem> findOne(String exactName)
	{
		List<FoodCommonEntity> list = repository.findByDeletedIsFalseAndName(exactName);
		return list.isEmpty() ? null : convert(list.get(0));
	}

	public SortedMap<String, String> getDataHashes()
	{
		// TODO: check why Sorted is required
		// TODO: check performance
		// TODO: probably storing entries is unnecessary, so we should process it as we go

		Map<String, String> result = repository.findAll().stream().collect(toMap(FoodCommonEntity::getId, FoodCommonEntity::getHash));
		return new TreeMap<>(result);
	}
}
