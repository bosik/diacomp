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

import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.persistence.parsers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerMap;
import org.bosik.diacomp.core.persistence.utils.ParserVersioned;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.features.user.auth.UserRest;
import org.bosik.merklesync.DataSource;
import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.ws.rs.core.MediaType;
import java.util.Date;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/diary")
public class DiaryRest extends UserRest
{
	private static final String TYPE_JSON_UTF8    = MediaType.APPLICATION_JSON + ";charset=utf-8";
	private static final int    MAX_DATETIME_SIZE = Utils.FORMAT_DATE_TIME.length();

	private final Serializer<Versioned<DiaryRecord>> serializer    = new SerializerAdapter<>(
			new ParserVersioned<>(new ParserDiaryRecord()));
	private final Serializer<Map<String, String>>    serializerMap = new SerializerMap();

	@Autowired
	private DiaryLocalService diaryService;

	@GetMapping("/count")
	public Integer count()
	{
		final int userId = getUserId();
		return diaryService.count(userId);
	}

	@GetMapping("/foodstat")
	public Map<String, Double> getFoodStatistics(
			@RequestParam("from") String parTimeFrom,
			@RequestParam("to") String parTimeTo)
	{
		final int userId = getUserId();
		final Date timeFrom = Utils.parseTimeUTC(parTimeFrom);
		final Date timeTo = Utils.parseTimeUTC(parTimeTo);

		return diaryService.getFoodStatistics(userId, timeFrom, timeTo);
	}

	@GetMapping("/count/{prefix}")
	public Integer count(@PathVariable(name = "prefix") String prefix)
	{
		final int userId = getUserId();

		Utils.checkSize(prefix, ObjectService.ID_FULL_SIZE);
		return diaryService.count(userId, prefix);
	}

	@GetMapping(path = "/guid", produces = TYPE_JSON_UTF8)
	public List<Versioned<DiaryRecord>> findById()
	{
		final int userId = getUserId();
		return diaryService.findAll(userId, true);
	}

	@GetMapping(path = "/guid/{prefix}", produces = TYPE_JSON_UTF8)
	public Object findById(@PathVariable(name = "prefix") String prefix)
	{
		final int userId = getUserId();
		Utils.checkSize(prefix, ObjectService.ID_FULL_SIZE);

		if (prefix.length() <= DataSource.ID_PREFIX_SIZE)
		{
			return diaryService.findByIdPrefix(userId, prefix);
		}
		else
		{
			Versioned<DiaryRecord> item = diaryService.findById(userId, prefix);

			if (item != null)
			{
				return item;
			}
			else
			{
				throw new NotFoundException(prefix);
			}
		}
	}

	@GetMapping(path = "/changes", produces = TYPE_JSON_UTF8)
	public List<Versioned<DiaryRecord>> findChanged(@RequestParam("since") String parTime)
	{
		final int userId = getUserId();
		Utils.checkSize(parTime, Utils.FORMAT_DATE_TIME.length());

		Date since = Utils.parseTimeUTC(parTime);
		return diaryService.findChanged(userId, since);
	}

	@GetMapping(path = "/period", produces = TYPE_JSON_UTF8)
	public List<Versioned<DiaryRecord>> findPeriod(@RequestParam(name = "start_time") String parStartTime,
			@RequestParam(name = "end_time") String parEndTime,
			@RequestParam(name = "show_rem", defaultValue = "false") boolean includeRemoved)
	{
		final int userId = getUserId();
		Utils.checkSize(parStartTime, MAX_DATETIME_SIZE);
		Utils.checkSize(parEndTime, MAX_DATETIME_SIZE);

		Date startTime = Utils.parseTimeUTC(parStartTime);
		Date endTime = Utils.parseTimeUTC(parEndTime);

		return diaryService.findPeriod(userId, startTime, endTime, includeRemoved);
	}

	@GetMapping(path = "/hash")
	public String getHash()
	{
		final int userId = getUserId();
		MerkleTree hashTree = diaryService.getHashTree(userId);
		return Utils.nullToEmpty(hashTree.getHash(""));
	}

	@GetMapping(path = "/hash/{prefix}")
	public String getHash(@PathVariable(name = "prefix") String prefix)
	{
		final int userId = getUserId();
		Utils.checkSize(prefix, ObjectService.ID_FULL_SIZE);

		MerkleTree hashTree = diaryService.getHashTree(userId);
		return Utils.nullToEmpty(hashTree.getHash(prefix));
	}

	@GetMapping(path = "/hashes", produces = TYPE_JSON_UTF8)
	public String getHashChildren()
	{
		final int userId = getUserId();
		MerkleTree hashTree = diaryService.getHashTree(userId);
		Map<String, String> map = hashTree.getHashChildren("");
		return serializerMap.write(map);
	}

	@GetMapping(path = "/hashes/{prefix}", produces = TYPE_JSON_UTF8)
	public String getHashChildren(@PathVariable(name = "prefix") String prefix)
	{
		final int userId = getUserId();
		Utils.checkSize(prefix, ObjectService.ID_FULL_SIZE);

		MerkleTree hashTree = diaryService.getHashTree(userId);
		Map<String, String> map = hashTree.getHashChildren(prefix);
		return serializerMap.write(map);
	}

	@PutMapping
	public String save(@RequestParam("items") String parItems)
	{
		final int userId = getUserId();

		// FIXME: limit the maximum data size
		List<Versioned<DiaryRecord>> items = serializer.readAll(Utils.removeNonUtf8(parItems));
		diaryService.save(userId, items);
		return "Saved OK";
	}
}