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
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerFoodItem;
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
@RequestMapping("/food/user")
public class FoodUserRest extends UserRest
{
	private static final String TYPE_JSON_UTF8 = MediaType.APPLICATION_JSON + ";charset=utf-8";

	private final Serializer<Versioned<FoodItem>> serializer    = new SerializerFoodItem();
	private final Serializer<Map<String, String>> serializerMap = new SerializerMap();

	@Autowired
	private FoodUserLocalService foodUserService;

	@GetMapping("/count")
	public int count()
	{
		final int userId = getUserId();

		return foodUserService.count(userId);
	}

	@GetMapping("/count/{prefix}")
	public int count(@PathVariable(name = "prefix") String prefix)
	{
		final int userId = getUserId();
		Utils.checkSize(prefix, ObjectService.ID_FULL_SIZE);

		return foodUserService.count(userId, prefix);
	}

	@GetMapping(path = "/guid", produces = TYPE_JSON_UTF8)
	public List<Versioned<FoodItem>> findById()
	{
		final int userId = getUserId();

		return foodUserService.findAll(userId, true);
	}

	@GetMapping(path = "/guid/{prefix}", produces = TYPE_JSON_UTF8)
	public Object findById(@PathVariable(name = "prefix") String prefix)
	{
		final int userId = getUserId();
		Utils.checkSize(prefix, ObjectService.ID_FULL_SIZE);

		if (prefix.length() <= DataSource.ID_PREFIX_SIZE)
		{
			return foodUserService.findByIdPrefix(userId, prefix);
		}
		else
		{
			Versioned<FoodItem> item = foodUserService.findById(userId, prefix);

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
	public List<Versioned<FoodItem>> findAll(@RequestParam(value = "show_rem", defaultValue = "false") String parShowRem)
	{
		final int userId = getUserId();
		Utils.checkSize(parShowRem, 5); // "false".length
		boolean includeRemoved = Boolean.valueOf(parShowRem);

		return foodUserService.findAll(userId, includeRemoved);
	}

	@GetMapping(path = "/search", produces = TYPE_JSON_UTF8)
	public List<Versioned<FoodItem>> findAny(@RequestParam("q") String filter)
	{
		final int userId = getUserId();
		Utils.checkSize(filter, 256);

		return foodUserService.findAny(userId, filter);
	}

	@GetMapping(path = "/changes", produces = TYPE_JSON_UTF8)
	public List<Versioned<FoodItem>> findChanged(@RequestParam("since") String parTime)
	{
		final int userId = getUserId();
		Utils.checkSize(parTime, Utils.FORMAT_DATE_TIME.length());
		Date since = Utils.parseTimeUTC(parTime);

		return foodUserService.findChanged(userId, since);
	}

	@GetMapping(path = "/hash")
	public String getHash()
	{
		final int userId = getUserId();

		MerkleTree hashTree = foodUserService.getHashTree(userId);
		return Utils.nullToEmpty(hashTree.getHash(""));
	}

	@GetMapping(path = "/hash/{prefix}")
	public String getHash(@PathVariable(name = "prefix") String prefix)
	{
		final int userId = getUserId();
		Utils.checkSize(prefix, ObjectService.ID_FULL_SIZE);

		MerkleTree hashTree = foodUserService.getHashTree(userId);
		return Utils.nullToEmpty(hashTree.getHash(prefix));
	}

	@GetMapping(path = "/hashes", produces = TYPE_JSON_UTF8)
	public String getHashChildren()
	{
		final int userId = getUserId();

		MerkleTree hashTree = foodUserService.getHashTree(userId);
		Map<String, String> map = hashTree.getHashChildren("");
		return serializerMap.write(map);
	}

	@GetMapping(path = "/hashes/{prefix}", produces = TYPE_JSON_UTF8)
	public String getHashChildren(@PathVariable(name = "prefix") String prefix)
	{
		final int userId = getUserId();
		Utils.checkSize(prefix, ObjectService.ID_FULL_SIZE);

		MerkleTree hashTree = foodUserService.getHashTree(userId);
		Map<String, String> map = hashTree.getHashChildren(prefix);
		return serializerMap.write(map);
	}

	@PutMapping
	public String save(@RequestParam(name = "items") String parItems)
	{
		final int userId = getUserId();
		List<Versioned<FoodItem>> items = serializer.readAll(Utils.removeNonUtf8(parItems));

		foodUserService.save(userId, items);
		return "Saved OK";
	}
}
