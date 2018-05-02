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

import java.util.Map;
import java.util.Random;
import java.util.concurrent.ConcurrentHashMap;

import static org.bosik.diacomp.core.utils.Utils.MsecPerMin;

public class CachedHashTree
{
	private static class CacheEntry<T>
	{
		private T    data;
		private long expirationTime;

		CacheEntry(T data, long expirationTime)
		{
			this.data = data;
			this.expirationTime = expirationTime;
		}

		public T getData()
		{
			return (System.nanoTime() < expirationTime) ? data : null;
		}
	}

	// FIXME
	private static final long MIN_TTL = 1 * MsecPerMin; // 1 * MsecPerHour;
	private static final long MAX_TTL = 3 * MsecPerMin; // 3 * MsecPerHour;

	private final Map<Integer, CacheEntry<MerkleTree>> cache = new ConcurrentHashMap<>();

	public MerkleTree get(int userId)
	{
		CacheEntry<MerkleTree> entry = cache.get(userId);
		return (entry != null) ? entry.getData() : null;
	}

	public void set(int userId, MerkleTree tree)
	{
		if (tree != null)
		{
			long ttl = MIN_TTL + new Random().nextInt((int) (MAX_TTL - MIN_TTL)); // ms
			cache.put(userId, new CacheEntry<>(tree, System.nanoTime() + ttl * 1_000_000L));
		}
		else
		{
			cache.remove(userId);
		}
	}
}
