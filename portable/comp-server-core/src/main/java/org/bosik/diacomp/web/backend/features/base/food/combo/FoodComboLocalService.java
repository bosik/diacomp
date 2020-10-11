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
package org.bosik.diacomp.web.backend.features.base.food.combo;

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserFoodItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonEntity;
import org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonEntityRepository;
import org.bosik.diacomp.web.backend.features.base.food.common.FoodCommonLocalService;
import org.bosik.diacomp.web.backend.features.base.food.user.FoodUserEntity;
import org.bosik.diacomp.web.backend.features.base.food.user.FoodUserEntityRepository;
import org.bosik.diacomp.web.backend.features.base.food.user.FoodUserLocalService;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;

@Service
public class FoodComboLocalService
{
	private static final Parser<FoodItem>     parser     = new ParserFoodItem();
	private static final Serializer<FoodItem> serializer = new SerializerAdapter<>(parser);

	@Autowired
	private FoodUserLocalService foodUserLocalService;

	@Autowired
	private FoodCommonLocalService foodCommonLocalService;

	@Autowired
	private CachedFoodComboHashTree cachedHashTree;

	@Autowired
	private FoodUserEntityRepository foodUserEntityRepository;

	@Autowired
	private FoodCommonEntityRepository foodCommonEntityRepository;

	// -------------------------------------------------------------------------------------------------

	@Deprecated
	private static List<Versioned<FoodItem>> merge(List<Versioned<FoodItem>> foodCommon, List<Versioned<FoodItem>> foodUser)
	{
		Map<String, Versioned<FoodItem>> map = new HashMap<>();
		for (Versioned<FoodItem> f : foodCommon)
		{
			map.put(f.getId(), f);
		}

		for (Versioned<FoodItem> f : foodUser) // user overrides common
		{
			map.put(f.getId(), f);
		}

		return new ArrayList<>(map.values());
	}

	public void add(int userId, Versioned<FoodItem> item) throws PersistenceException
	{
		save(userId, Collections.singletonList(item));
	}

	public int count(int userId)
	{
		return foodUserEntityRepository.countCombo(userId);
	}

	public int count(int userId, String prefix)
	{
		if (prefix == null)
		{
			throw new IllegalArgumentException("ID prefix is null");
		}

		return foodUserEntityRepository.countCombo(userId, prefix);
	}

	public void delete(int userId, String id)
	{
		Versioned<FoodItem> item = findById(userId, id);

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
		save(userId, Collections.singletonList(item));
	}

	public List<Versioned<FoodItem>> findAll(int userId, boolean includeRemoved)
	{
		List<Versioned<FoodItem>> foodCommon = foodCommonLocalService.findAll(true);
		List<Versioned<FoodItem>> foodUser = foodUserLocalService.findAll(userId, true);
		List<Versioned<FoodItem>> result = merge(foodCommon, foodUser);

		if (!includeRemoved)
		{
			result.removeIf(Versioned::isDeleted);
		}

		return result;
	}

	public List<Versioned<FoodItem>> findAny(int userId, String filter)
	{
		List<Versioned<FoodItem>> foodCommon = foodCommonLocalService.findAny(filter);
		List<Versioned<FoodItem>> foodUser = foodUserLocalService.findAny(userId, filter);
		return merge(foodCommon, foodUser);
	}

	public Versioned<FoodItem> findOne(int userId, String exactName)
	{
		Versioned<FoodItem> foodUser = foodUserLocalService.findOne(userId, exactName);

		if (foodUser != null)
		{
			return foodUser;
		}
		else
		{
			return foodCommonLocalService.findOne(exactName);
		}
	}

	public Versioned<FoodItem> findById(int userId, String id)
	{
		Versioned<FoodItem> foodUser = foodUserLocalService.findById(userId, id);

		if (foodUser != null)
		{
			return foodUser;
		}
		else
		{
			return foodCommonLocalService.findById(id);
		}
	}

	public List<Versioned<FoodItem>> findByIdPrefix(int userId, String prefix)
	{
		List<Versioned<FoodItem>> foodCommon = foodCommonLocalService.findByIdPrefix(prefix);
		List<Versioned<FoodItem>> foodUser = foodUserLocalService.findByIdPrefix(userId, prefix);
		return merge(foodCommon, foodUser);
	}

	public List<Versioned<FoodItem>> findChanged(int userId, Date since)
	{
		List<Versioned<FoodItem>> foodCommon = foodCommonLocalService.findChanged(since);
		List<Versioned<FoodItem>> foodUser = foodUserLocalService.findChanged(userId, since);
		return merge(foodCommon, foodUser);
	}

	/**
	 * @return Sorted map (ID, Hash) for all items
	 */
	private SortedMap<String, String> getDataHashes(int userId)
	{
		SortedMap<String, String> foodCommon = foodCommonLocalService.getDataHashes();
		SortedMap<String, String> foodUser = foodUserLocalService.getDataHashes(userId);

		foodCommon.putAll(foodUser);
		return foodCommon;
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

	public void save(int userId, List<Versioned<FoodItem>> items)
	{
		foodUserLocalService.save(userId, items);
	}

	public String exportJson(int userId)
	{
		final StringBuilder s = new StringBuilder();
		s.append("[");

		// fetch common food
		List<FoodCommonEntity> foodCommon = foodCommonEntityRepository.findNotOverridden(userId);
		for (FoodCommonEntity food : foodCommon)
		{
			if (s.length() > 1)
			{
				s.append(",");
			}

			s.append("{");
			s.append("\"id\":\"").append(food.getId()).append("\",");
			s.append("\"stamp\":\"").append(Utils.formatTimeUTC(food.getLastModified())).append("\",");
			s.append("\"hash\":\"").append(food.getHash()).append("\",");
			s.append("\"version\":").append(food.getVersion()).append(",");
			s.append("\"deleted\":").append(food.isDeleted()).append(",");
			s.append("\"data\":").append(serializer.write(convertToFoodItem(food)));
			s.append("}");
		}

		// fetch user food
		List<FoodUserEntity> foodUser = foodUserEntityRepository.findByIdUserId(userId);
		for (FoodUserEntity f : foodUser)
		{
			if (s.length() > 1)
			{
				s.append(",");
			}

			s.append("{");
			s.append("\"id\":\"").append(f.getId().getId()).append("\",");
			s.append("\"stamp\":\"").append(Utils.formatTimeUTC(f.getLastModified())).append("\",");
			s.append("\"hash\":\"").append(f.getHash()).append("\",");
			s.append("\"version\":").append(f.getVersion()).append(",");
			s.append("\"deleted\":").append(f.isDeleted()).append(",");
			s.append("\"data\":").append(serializer.write(convertToFoodItem(f)));
			s.append("}");
		}

		s.append("]");
		return s.toString();
	}

	public String exportPlain(int userId)
	{
		final StringBuilder s = new StringBuilder();
		s.append("VERSION=1\n");

		// fetch common food
		List<FoodCommonEntity> foodCommon = foodCommonEntityRepository.findNotOverridden(userId);
		for (FoodCommonEntity f : foodCommon)
		{
			s.append(Utils.removeTabs(f.getName())).append('\t');
			s.append(f.getId()).append('\t');
			s.append(Utils.formatTimeUTC(f.getLastModified())).append('\t');
			s.append(f.getHash()).append('\t');
			s.append(f.getVersion()).append('\t');
			s.append(f.isDeleted()).append('\t');
			s.append(serializer.write(convertToFoodItem(f))).append('\n');
		}

		// fetch user food
		List<FoodUserEntity> foodUser = foodUserEntityRepository.findByIdUserId(userId);
		for (FoodUserEntity f : foodUser)
		{
			s.append(Utils.removeTabs(f.getName())).append('\t');
			s.append(f.getId().getId()).append('\t');
			s.append(Utils.formatTimeUTC(f.getLastModified())).append('\t');
			s.append(f.getHash()).append('\t');
			s.append(f.getVersion()).append('\t');
			s.append(f.isDeleted()).append('\t');
			s.append(serializer.write(convertToFoodItem(f))).append('\n');
		}

		return s.toString();
	}

	private static FoodItem convertToFoodItem(FoodUserEntity entity)
	{
		FoodItem food = new FoodItem();

		food.setName(entity.getName());
		food.setRelProts(entity.getProts());
		food.setRelFats(entity.getFats());
		food.setRelCarbs(entity.getCarbs());
		food.setRelValue(entity.getValue());
		food.setFromTable(entity.isFromTable());

		return food;
	}

	private static FoodItem convertToFoodItem(FoodCommonEntity entity)
	{
		FoodItem food = new FoodItem();

		food.setName(entity.getName());
		food.setRelProts(entity.getProts());
		food.setRelFats(entity.getFats());
		food.setRelCarbs(entity.getCarbs());
		food.setRelValue(entity.getValue());
		food.setFromTable(entity.isFromTable());

		return food;
	}
}
