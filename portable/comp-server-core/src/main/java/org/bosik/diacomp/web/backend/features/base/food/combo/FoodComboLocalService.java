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
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Optional;
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
	private FoodCommonEntityRepository foodCommonEntityRepository;

	@Autowired
	private FoodUserEntityRepository foodUserEntityRepository;

	// -------------------------------------------------------------------------------------------------

	public void add(int userId, Versioned<FoodItem> item) throws PersistenceException
	{
		save(userId, Collections.singletonList(item));
	}

	public int count(int userId)
	{
		return foodUserEntityRepository.countByKeyUserId(userId) +
				foodCommonEntityRepository.countOrg(userId);
	}

	public int count(int userId, String prefix)
	{
		if (prefix == null)
		{
			throw new IllegalArgumentException("ID prefix is null");
		}

		return foodUserEntityRepository.countByKeyUserIdAndKeyIdStartingWith(userId, prefix) +
				foodCommonEntityRepository.countOrgByIdStartingWith(userId, prefix);
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
		final List<FoodEntity> entities = includeRemoved

				? combineSorted(
				foodCommonEntityRepository.findOrg(userId),
				foodUserEntityRepository.findByKeyUserId(userId))

				: combineSorted(
				foodCommonEntityRepository.findOrgByDeletedIsFalse(userId),
				foodUserEntityRepository.findByKeyUserIdAndDeletedIsFalse(userId));

		return FoodUserLocalService.convert(entities);
	}

	public List<Versioned<FoodItem>> findAny(int userId, String filter)
	{
		final List<FoodEntity> entities = combineSorted(
				foodCommonEntityRepository.findOrgByDeletedIsFalseAndNameContaining(userId, filter),
				foodUserEntityRepository.findByKeyUserIdAndDeletedIsFalseAndNameContaining(userId, filter));

		return FoodUserLocalService.convert(entities);
	}

	public Versioned<FoodItem> findOne(int userId, String exactName)
	{
		final List<FoodUserEntity> user = foodUserEntityRepository.findByKeyUserIdAndName(userId, exactName);

		// user has such food, non-deleted
		final Optional<FoodUserEntity> userNonDeleted = user.stream()
				.filter(e -> !e.isDeleted())
				.findAny();

		if (userNonDeleted.isPresent())
		{
			return FoodUserLocalService.convert(userNonDeleted.get());
		}

		// user has such food, but it's deleted
		if (!user.isEmpty())
		{
			return null;
		}

		// check common base
		final List<FoodCommonEntity> common = foodCommonEntityRepository.findByDeletedIsFalseAndName(exactName);
		if (!common.isEmpty())
		{
			return FoodUserLocalService.convert(common.get(0));
		}

		return null;
	}

	public Versioned<FoodItem> findById(int userId, String id)
	{
		final FoodUserEntity foodUser = foodUserEntityRepository.findByKeyUserIdAndKeyId(userId, id);
		if (foodUser != null)
		{
			return FoodUserLocalService.convert(foodUser);
		}

		final FoodCommonEntity foodCommon = foodCommonEntityRepository.findById(id).orElse(null);
		return FoodUserLocalService.convert(foodCommon);
	}

	public List<Versioned<FoodItem>> findByIdPrefix(int userId, String prefix)
	{
		return FoodUserLocalService.convert(combine(
				foodUserEntityRepository.findByKeyUserIdAndKeyIdStartingWith(userId, prefix),
				foodCommonEntityRepository.findOrgByIdStartingWith(userId, prefix)));
	}

	public List<Versioned<FoodItem>> findChanged(int userId, Date since)
	{
		return FoodUserLocalService.convert(combine(
				foodUserEntityRepository.findByKeyUserIdAndLastModifiedIsGreaterThanEqual(userId, since),
				foodCommonEntityRepository.findOrgByLastModifiedIsGreaterThanEqual(userId, since)));
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

		for (FoodEntity food : combine(
				foodCommonEntityRepository.findOrg(userId),
				foodUserEntityRepository.findByKeyUserId(userId)
		))
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

		s.append("]");
		return s.toString();
	}

	public String exportPlain(int userId)
	{
		final StringBuilder s = new StringBuilder();
		s.append("VERSION=1\n");

		for (FoodEntity food : combine(
				foodCommonEntityRepository.findOrg(userId),
				foodUserEntityRepository.findByKeyUserId(userId)
		))
		{
			s.append(Utils.removeTabs(food.getName())).append('\t');
			s.append(food.getId()).append('\t');
			s.append(Utils.formatTimeUTC(food.getLastModified())).append('\t');
			s.append(food.getHash()).append('\t');
			s.append(food.getVersion()).append('\t');
			s.append(food.isDeleted()).append('\t');
			s.append(serializer.write(convertToFoodItem(food))).append('\n');
		}

		return s.toString();
	}

	private static FoodItem convertToFoodItem(FoodEntity entity)
	{
		final FoodItem food = new FoodItem();

		food.setName(entity.getName());
		food.setRelProts(entity.getProts());
		food.setRelFats(entity.getFats());
		food.setRelCarbs(entity.getCarbs());
		food.setRelValue(entity.getValue());
		food.setFromTable(entity.isFromTable());

		return food;
	}

	private static <T> List<T> combine(List<? extends T> a, List<? extends T> b)
	{
		final List<T> result = new ArrayList<>(a.size() + b.size());
		result.addAll(a);
		result.addAll(b);
		return result;
	}

	private static <T extends FoodEntity> List<T> combineSorted(List<? extends T> a, List<? extends T> b)
	{
		final List<T> combined = combine(a, b);
		combined.sort(Comparator.comparing(FoodEntity::getName));
		return combined;
	}
}
