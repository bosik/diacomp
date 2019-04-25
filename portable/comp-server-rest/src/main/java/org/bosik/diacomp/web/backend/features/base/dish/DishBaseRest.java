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
import org.bosik.diacomp.core.services.exceptions.TooManyItemsException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.features.user.auth.UserRest;
import org.bosik.merklesync.DataSource;
import org.bosik.merklesync.MerkleTree;
import org.bosik.merklesync.Versioned;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
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
	private static final String                          TYPE_JSON_UTF8 = MediaType.APPLICATION_JSON + ";charset=utf-8";
	private final        Serializer<Map<String, String>> serializerMap  = new SerializerMap();

	@Autowired
	private DishBaseLocalService dishbaseService;

	private final Serializer<Versioned<DishItem>> serializer = new SerializerDishItem();

	@GetMapping(path = { "/count", "/count/{prefix}" })
	public Integer count(@PathVariable(name = "prefix", required = false) String parPrefix)
	{
		Utils.checkSize(parPrefix, ObjectService.ID_FULL_SIZE);

		if (parPrefix != null)
		{
			return dishbaseService.count(getUserId(), parPrefix);
		}
		else
		{
			return dishbaseService.count(getUserId());
		}
	}

	@GetMapping(path = { "/guid", "/guid/{prefix}" }, produces = TYPE_JSON_UTF8)
	public ResponseEntity findById(@PathVariable(name = "prefix", required = false) String prefix)
	{
		try
		{
			if (prefix != null)
			{
				Utils.checkSize(prefix, ObjectService.ID_FULL_SIZE);

				if (prefix.length() <= DataSource.ID_PREFIX_SIZE)
				{
					List<Versioned<DishItem>> items = dishbaseService.findByIdPrefix(getUserId(), prefix);
					return ResponseEntity.ok(items);
				}
				else
				{
					Versioned<DishItem> item = dishbaseService.findById(getUserId(), prefix);

					if (item != null)
					{
						return ResponseEntity.ok(item);
					}
					else
					{
						String response = String.format("Item %s not found", prefix);
						return ResponseEntity.status(HttpStatus.NOT_FOUND).body(response);
					}
				}
			}
			else
			{
				List<Versioned<DishItem>> items = dishbaseService.findAll(getUserId(), true);
				return ResponseEntity.ok(items);
			}
		}
		catch (TooManyItemsException e)
		{
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("Too many items found");
		}
	}

	@GetMapping(path = "/all", produces = TYPE_JSON_UTF8)
	public List<Versioned<DishItem>> findAll(@RequestParam(value = "show_rem", defaultValue = "false") String parShowRem)
	{
		Utils.checkSize(parShowRem, 5); // "false".length

		boolean includeRemoved = Boolean.valueOf(parShowRem);
		return dishbaseService.findAll(getUserId(), includeRemoved);
	}

	@GetMapping(path = "/search", produces = TYPE_JSON_UTF8)
	public List<Versioned<DishItem>> findAny(@RequestParam("q") String filter)
	{
		Utils.checkSize(filter, 256);

		return dishbaseService.findAny(getUserId(), filter);
	}

	@GetMapping(path = "/changes", produces = TYPE_JSON_UTF8)
	public List<Versioned<DishItem>> findChanged(@RequestParam("since") String parTime)
	{
		Utils.checkSize(parTime, Utils.FORMAT_DATE_TIME.length());

		Date since = Utils.parseTimeUTC(parTime);
		return dishbaseService.findChanged(getUserId(), since);
	}

	@GetMapping(path = { "/hash", "/hash/{prefix}" }, produces = TYPE_JSON_UTF8)
	public String getHash(@PathVariable(name = "prefix", required = false) String parPrefix)
	{
		parPrefix = Utils.nullToEmpty(parPrefix);
		Utils.checkSize(parPrefix, ObjectService.ID_FULL_SIZE);

		MerkleTree hashTree = dishbaseService.getHashTree(getUserId());
		String s = hashTree.getHash(parPrefix);
		return Utils.nullToEmpty(s);
	}

	@GetMapping(path = { "/hashes", "/hashes/{prefix}" }, produces = TYPE_JSON_UTF8)
	public String getHashChildren(@PathVariable(name = "prefix", required = false) String parPrefix)
	{
		parPrefix = Utils.nullToEmpty(parPrefix);
		Utils.checkSize(parPrefix, ObjectService.ID_FULL_SIZE);

		MerkleTree hashTree = dishbaseService.getHashTree(getUserId());
		Map<String, String> map = hashTree.getHashChildren(parPrefix);
		return serializerMap.write(map);
	}

	@PutMapping(produces = TYPE_JSON_UTF8)
	public String save(@RequestParam(name = "items") String parItems)
	{
		List<Versioned<DishItem>> items = serializer.readAll(Utils.removeNonUtf8(parItems));
		dishbaseService.save(getUserId(), items);
		return "Saved OK";
	}
}
