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
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerDishItem;
import org.bosik.diacomp.core.persistence.serializers.SerializerMap;
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
@RequestMapping("/dish")
public class DishBaseRest extends UserRest
{
	private static final String TYPE_JSON_UTF8 = MediaType.APPLICATION_JSON + ";charset=utf-8";

	private final Serializer<Map<String, String>> serializerMap = new SerializerMap();
	private final Serializer<Versioned<DishItem>> serializer    = new SerializerDishItem();

	@Autowired
	private DishBaseLocalService dishbaseService;

	@GetMapping("/count")
	public Integer count()
	{
		return dishbaseService.count(getUserId());
	}

	@GetMapping("/count/{prefix}")
	public Integer count(@PathVariable("prefix") String prefix)
	{
		final int userId = getUserId();
		Utils.checkSize(prefix, ObjectService.ID_FULL_SIZE);

		return dishbaseService.count(userId, prefix);
	}

	@GetMapping(path = "/guid", produces = TYPE_JSON_UTF8)
	public List<Versioned<DishItem>> findById()
	{
		final int userId = getUserId();

		return dishbaseService.findAll(userId, true);
	}

	@GetMapping(path = "/guid/{prefix}", produces = TYPE_JSON_UTF8)
	public Object findById(@PathVariable("prefix") String prefix)
	{
		final int userId = getUserId();
		Utils.checkSize(prefix, ObjectService.ID_FULL_SIZE);

		if (prefix.length() <= DataSource.ID_PREFIX_SIZE)
		{
			return dishbaseService.findByIdPrefix(userId, prefix);
		}
		else
		{
			Versioned<DishItem> item = dishbaseService.findById(userId, prefix);

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

	@GetMapping(path = "/all", produces = TYPE_JSON_UTF8)
	public List<Versioned<DishItem>> findAll(@RequestParam(value = "show_rem", defaultValue = "false") String parShowRem)
	{
		final int userId = getUserId();
		Utils.checkSize(parShowRem, 5); // "false".length
		boolean includeRemoved = Boolean.valueOf(parShowRem);

		return dishbaseService.findAll(userId, includeRemoved);
	}

	@GetMapping(path = "/search", produces = TYPE_JSON_UTF8)
	public List<Versioned<DishItem>> findAny(@RequestParam("q") String filter)
	{
		final int userId = getUserId();
		Utils.checkSize(filter, 256);

		return dishbaseService.findAny(userId, filter);
	}

	@GetMapping(path = "/changes", produces = TYPE_JSON_UTF8)
	public List<Versioned<DishItem>> findChanged(@RequestParam("since") String parTime)
	{
		final int userId = getUserId();
		Utils.checkSize(parTime, Utils.FORMAT_DATE_TIME.length());
		Date since = Utils.parseTimeUTC(parTime);

		return dishbaseService.findChanged(userId, since);
	}

	@GetMapping(path = "/hash")
	public String getHash()
	{
		final int userId = getUserId();

		final MerkleTree hashTree = dishbaseService.getHashTree(userId);
		return Utils.nullToEmpty(hashTree.getHash(""));
	}

	@GetMapping(path = "/hash/{prefix}")
	public String getHash(@PathVariable("prefix") String prefix)
	{
		final int userId = getUserId();
		Utils.checkSize(prefix, ObjectService.ID_FULL_SIZE);

		MerkleTree hashTree = dishbaseService.getHashTree(userId);
		return Utils.nullToEmpty(hashTree.getHash(prefix));
	}

	@GetMapping(path = "/hashes", produces = TYPE_JSON_UTF8)
	public String getHashChildren()
	{
		final int userId = getUserId();

		MerkleTree hashTree = dishbaseService.getHashTree(userId);
		Map<String, String> map = hashTree.getHashChildren("");
		return serializerMap.write(map);
	}

	@GetMapping(path = "/hashes/{prefix}", produces = TYPE_JSON_UTF8)
	public String getHashChildren(@PathVariable("prefix") String prefix)
	{
		final int userId = getUserId();
		Utils.checkSize(prefix, ObjectService.ID_FULL_SIZE);

		MerkleTree hashTree = dishbaseService.getHashTree(userId);
		Map<String, String> map = hashTree.getHashChildren(prefix);
		return serializerMap.write(map);
	}

	@PutMapping
	public String save(@RequestParam("items") String parItems)
	{
		final int userId = getUserId();

		List<Versioned<DishItem>> items = serializer.readAll(Utils.removeNonUtf8(parItems));
		dishbaseService.save(userId, items);
		return "Saved OK";
	}
}
