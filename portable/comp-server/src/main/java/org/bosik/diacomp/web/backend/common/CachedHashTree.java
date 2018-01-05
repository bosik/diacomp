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
package org.bosik.diacomp.web.backend.common;

import org.bosik.merklesync.MerkleTree;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * We can't use session scope here, otherwise e.g. parallel syncing desktop + mobile clients will not work (each will use it's own cache copy)
 */

@Service
//@Scope(value = "session", proxyMode = ScopedProxyMode.TARGET_CLASS)
public class CachedHashTree
{
	private final Map<Integer, MerkleTree> mapDiaryTree = new ConcurrentHashMap<>();
	private final Map<Integer, MerkleTree> mapFoodTree  = new ConcurrentHashMap<>();
	private final Map<Integer, MerkleTree> mapDishTree  = new ConcurrentHashMap<>();

	public MerkleTree getDiaryTree(int userId)
	{
		return mapDiaryTree.get(userId);
	}

	public void setDiaryTree(int userId, MerkleTree tree)
	{
		put(mapDiaryTree, userId, tree);
	}

	public MerkleTree getFoodTree(int userId)
	{
		return mapFoodTree.get(userId);
	}

	public void setFoodTree(int userId, MerkleTree tree)
	{
		put(mapFoodTree, userId, tree);
	}

	public MerkleTree getDishTree(int userId)
	{
		return mapDishTree.get(userId);
	}

	public void setDishTree(int userId, MerkleTree tree)
	{
		put(mapDishTree, userId, tree);
	}

	private static <K, V> void put(Map<K, V> map, K key, V value)
	{
		if (value != null)
		{
			map.put(key, value);
		}
		else
		{
			map.remove(key);
		}
	}
}
