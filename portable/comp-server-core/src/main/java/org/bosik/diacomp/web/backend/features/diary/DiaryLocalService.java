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
package org.bosik.diacomp.web.backend.features.diary;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.UserDataService;
import org.bosik.diacomp.web.backend.features.base.dish.DishBaseLocalService;
import org.bosik.diacomp.web.backend.features.base.dish.DishEntityRepository;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.SortedMap;
import java.util.TimeZone;
import java.util.TreeMap;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

@Service
@NoArgsConstructor
@AllArgsConstructor
public class DiaryLocalService implements UserDataService<DiaryRecord>
{
	private static final Parser<DiaryRecord>     parser     = new ParserDiaryRecord();
	private static final Serializer<DiaryRecord> serializer = new SerializerAdapter<>(parser);

	@Autowired
	private DiaryEntityRepository repository;

	@Autowired
	private DishEntityRepository dishEntityRepository;

	@Autowired
	private CachedDiaryHashTree cachedHashTree;

	private static Versioned<DiaryRecord> convert(DiaryEntity e)
	{
		if (e == null)
		{
			return null;
		}

		final Versioned<DiaryRecord> result = new Versioned<>();

		result.setId(e.getId());
		result.setTimeStamp(e.getTimeStamp());
		result.setHash(e.getHash());
		result.setVersion(e.getVersion());
		result.setDeleted(e.isDeleted());
		result.setData(serializer.read(e.getContent()));

		return result;
	}

	private static List<Versioned<DiaryRecord>> convert(List<DiaryEntity> list)
	{
		return list.stream().map(DiaryLocalService::convert).collect(toList());
	}

	private static void copyData(Versioned<DiaryRecord> source, DiaryEntity destination)
	{
		destination.setTimeStamp(source.getTimeStamp());
		destination.setHash(source.getHash());
		destination.setVersion(source.getVersion());
		destination.setDeleted(source.isDeleted());
		destination.setContent(serializer.write(source.getData()));
		destination.setTimeCache(source.getData().getTime());
	}

	private static boolean verify(Versioned<DiaryRecord> record)
	{
		if (record != null && record.getId() != null && record.getId().length() == ObjectService.ID_FULL_SIZE)
		{
			record.setId(record.getId().toLowerCase());
			return true;
		}
		else
		{
			return false;
		}
	}

	@Override
	public int count(int userId)
	{
		return repository.countByUserId(userId);
	}

	@Override
	public int count(int userId, String prefix)
	{
		return repository.countByUserIdAndIdStartingWith(userId, prefix);
	}

	public void delete(int userId, String id)
	{
		final DiaryEntity entity = repository.findByUserIdAndId(userId, id);

		if (entity != null && !entity.isDeleted())
		{
			final Versioned<DiaryRecord> item = convert(entity);
			item.setDeleted(true);
			item.modified();
			save(userId, Collections.singletonList(item));
		}
	}

	@Override
	public List<Versioned<DiaryRecord>> findAll(int userId, boolean includeRemoved)
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

	public Map<String, Double> getFoodStatistics(int userId, Date from, Date to)
	{
		final List<DiaryEntity> entities = repository.findByUserIdAndTimeCacheBetweenAndDeletedIsFalseOrderByTimeCache(
				userId, from, to);
		final List<FoodMassed> diaryFood = convert(entities).stream()
				.filter(v -> v.getData() instanceof MealRecord)
				.flatMap(v -> ((MealRecord) v.getData()).getItems().stream())
				.collect(toList());

		final List<Versioned<DishItem>> dishes = DishBaseLocalService.convert(
				dishEntityRepository.findByUserIdAndDeletedIsFalse(userId));

		final Map<String, Double> stat = new HashMap<>();

		for (FoodMassed food : diaryFood)
		{
			final Optional<Versioned<DishItem>> dish = dishes.stream()
					.filter(d -> d.getData().getName().equals(food.getName()))
					.findFirst();

			if (dish.isPresent())
			{
				for (FoodMassed dishItem : dish.get().getData().getContent())
				{
					final String name = dishItem.getName();
					double mass = food.getMass() / dish.get().getData().getRealMass() * dishItem.getMass();

					stat.putIfAbsent(name, 0.0);
					stat.put(name, stat.getOrDefault(name, 0.0) + mass);
				}
			}
			else
			{
				final String name = food.getName();
				double mass = food.getMass();

				stat.putIfAbsent(name, 0.0);
				stat.put(name, stat.getOrDefault(name, 0.0) + mass);
			}
		}

		return stat;
	}

	@Override
	public Versioned<DiaryRecord> findById(int userId, String id)
	{
		return convert(repository.findByUserIdAndId(userId, id));
	}

	@Override
	public List<Versioned<DiaryRecord>> findByIdPrefix(int userId, String prefix)
	{
		return convert(repository.findByUserIdAndIdStartingWith(userId, prefix));
	}

	@Override
	public List<Versioned<DiaryRecord>> findChanged(int userId, Date time)
	{
		return convert(repository.findByUserIdAndTimeStampIsGreaterThanEqual(userId, time));
	}

	public List<Versioned<DiaryRecord>> findPeriod(int userId, Date startTime, Date endTime, boolean includeRemoved)
	{
		if (includeRemoved)
		{
			return convert(repository.findByUserIdAndTimeCacheBetweenOrderByTimeCache(userId, startTime, endTime));
		}
		else
		{
			return convert(repository.findByUserIdAndTimeCacheBetweenAndDeletedIsFalseOrderByTimeCache(userId, startTime, endTime));
		}
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
	public void save(int userId, List<Versioned<DiaryRecord>> records)
	{
		for (Versioned<DiaryRecord> item : records)
		{
			if (!verify(item))
			{
				throw new IllegalArgumentException("Invalid record: " + item);
			}

			DiaryEntity entity = repository.findByUserIdAndId(userId, item.getId());

			if (entity == null)
			{
				entity = new DiaryEntity();
				entity.setUserId(userId);
				entity.setId(item.getId());
			}

			copyData(item, entity);
			repository.save(entity);
			cachedHashTree.set(userId, null); // done in loop to reduce inconsistency window
		}
	}

	/**
	 * @return Sorted map (ID, Hash) for all items
	 */
	private SortedMap<String, String> getDataHashes(int userId)
	{
		// TODO: check why Sorted is required
		// TODO: check performance
		Map<String, String> result = repository.findByUserId(userId).stream().collect(toMap(DiaryEntity::getId, DiaryEntity::getHash));
		return new TreeMap<>(result);
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
		s.append("VERSION=6\n");

		// TODO: check performance
		repository.findByUserId(userId).forEach(entity ->
		{
			s.append(Utils.formatTimeUTC(entity.getTimeCache())).append('\t');
			s.append(entity.getId()).append('\t');
			s.append(Utils.formatTimeUTC(entity.getTimeStamp())).append('\t');
			s.append(entity.getHash()).append('\t');
			s.append(entity.getVersion()).append('\t');
			s.append(entity.isDeleted() ? "true" : "false").append('\t');
			s.append(Utils.removeTabs(entity.getContent())).append('\n');
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
				System.out.println(entity.getId() + "\t" + entity.getUserId() + "\t" + Utils
						.formatTimeLocal(TimeZone.getDefault(), entity.getTimeCache()) + "\t" + entity.getContent());
			}
		});
	}
}
