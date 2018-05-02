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
import java.util.Random;
import java.util.concurrent.ConcurrentHashMap;

import static org.bosik.diacomp.core.utils.Utils.MsecPerHour;

/**
 * We can't use session scope here, otherwise e.g. parallel syncing desktop + mobile clients will not work (each will use it's own cache copy)
 */

@Service
//@Scope(value = "session", proxyMode = ScopedProxyMode.TARGET_CLASS)
public class CachedHashTree
{
	private static class CacheEntry<T>
	{
		private T    data;
		private long expirationTime;

		public CacheEntry(T data, long expirationTime)
		{
			this.data = data;
			this.expirationTime = expirationTime;
		}

		public T getData()
		{
			return (System.nanoTime() < expirationTime) ? data : null;
		}
	}

	private static final long MIN_TTL = 1 * MsecPerHour;
	private static final long MAX_TTL = 3 * MsecPerHour;

	private final Map<Integer, CacheEntry<MerkleTree>> mapDiaryTree = new ConcurrentHashMap<>();
	private final Map<Integer, CacheEntry<MerkleTree>> mapFoodTree  = new ConcurrentHashMap<>();
	private final Map<Integer, CacheEntry<MerkleTree>> mapDishTree  = new ConcurrentHashMap<>();

	public MerkleTree getDiaryTree(int userId)
	{
		return get(mapDiaryTree, userId);
	}

	public void setDiaryTree(int userId, MerkleTree tree)
	{
		put(mapDiaryTree, userId, tree);
	}

	public MerkleTree getFoodTree(int userId)
	{
		return get(mapFoodTree, userId);
	}

	public void setFoodTree(int userId, MerkleTree tree)
	{
		put(mapFoodTree, userId, tree);
	}

	public MerkleTree getDishTree(int userId)
	{
		return get(mapDishTree, userId);
	}

	public void setDishTree(int userId, MerkleTree tree)
	{
		put(mapDishTree, userId, tree);
	}

	private static MerkleTree get(Map<Integer, CacheEntry<MerkleTree>> tree, int userId)
	{
		final CacheEntry<MerkleTree> entry = tree.get(userId);
		return (entry != null) ? entry.getData() : null;
	}

	private static void put(Map<Integer, CacheEntry<MerkleTree>> map, Integer key, MerkleTree tree)
	{
		if (tree != null)
		{
			final long ttl = MIN_TTL + new Random().nextInt((int) (MAX_TTL - MIN_TTL)); // ms
			map.put(key, new CacheEntry<>(tree, System.nanoTime() + ttl * 1_000_000L));
		}
		else
		{
			map.remove(key);
		}
	}
}
