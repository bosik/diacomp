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

import org.bosik.merklesync.MemoryMerkleTree3;
import org.bosik.merklesync.MerkleTree;
import org.junit.Assert;
import org.junit.Test;

import java.util.HashMap;
import java.util.Random;

public class TestCachedHashTree
{
	private static int buildUserId()
	{
		return new Random().nextInt();
	}

	private static MerkleTree buildTree()
	{
		return new MemoryMerkleTree3(new HashMap<String, String>());
	}

	private static void set(CachedHashTree cache, int userId, MerkleTree treeDiary, MerkleTree treeFood, MerkleTree treeDish)
	{
		cache.setDiaryTree(userId, treeDiary);
		cache.setFoodTree(userId, treeFood);
		cache.setDishTree(userId, treeDish);
	}

	private static void check(CachedHashTree cache, int userId, MerkleTree treeDiary, MerkleTree treeFood, MerkleTree treeDish)
	{
		Assert.assertEquals("Diary tree expectation failure, user " + userId, treeDiary, cache.getDiaryTree(userId));
		Assert.assertEquals("Food tree expectation failure, user " + userId, treeFood, cache.getFoodTree(userId));
		Assert.assertEquals("Dish tree expectation failure, user " + userId, treeDish, cache.getDishTree(userId));
	}

	// ------------------------------------------------------------------------------

	@Test
	public void test_initialized_with_null()
	{
		CachedHashTree cache = new CachedHashTree();

		int userId = buildUserId();
		check(cache, userId, null, null, null);
	}

	@Test
	public void test_can_store()
	{
		CachedHashTree cache = new CachedHashTree();

		MerkleTree treeDiary = buildTree();
		MerkleTree treeFood = buildTree();
		MerkleTree treeDish = buildTree();

		int userId = buildUserId();
		set(cache, userId, treeDiary, treeFood, treeDish);
		check(cache, userId, treeDiary, treeFood, treeDish);
	}

	@Test
	public void test_can_clear()
	{
		CachedHashTree cache = new CachedHashTree();

		int userId = buildUserId();
		set(cache, userId, buildTree(), buildTree(), buildTree());
		set(cache, userId, null, null, null);
		check(cache, userId, null, null, null);
	}

	@Test
	public void test_independence()
	{
		CachedHashTree cache = new CachedHashTree();

		MerkleTree treeDiary1 = buildTree();
		MerkleTree treeFood1 = buildTree();
		MerkleTree treeDish1 = buildTree();

		MerkleTree treeDiary2 = buildTree();
		MerkleTree treeFood2 = buildTree();
		MerkleTree treeDish2 = buildTree();

		int userId1 = buildUserId() % 100000;
		int userId2 = userId1 + 1;

		// ----------------------------------------

		set(cache, userId1, treeDiary1, treeFood1, treeDish1);
		check(cache, userId2, null, null, null);

		set(cache, userId2, treeDiary2, treeFood2, treeDish2);
		check(cache, userId1, treeDiary1, treeFood1, treeDish1);

		set(cache, userId2, null, null, null);
		check(cache, userId1, treeDiary1, treeFood1, treeDish1);
	}
}
