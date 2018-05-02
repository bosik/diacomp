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

	// ------------------------------------------------------------------------------

	@Test
	public void test_initialized_with_null()
	{
		int userId = buildUserId();

		CachedHashTree cache = new CachedHashTree();
		Assert.assertEquals("Hash tree expectation failure, user " + userId, null, cache.get(userId));
	}

	@Test
	public void test_can_store()
	{
		int userId = buildUserId();

		CachedHashTree cache = new CachedHashTree();
		MerkleTree tree = buildTree();
		cache.set(userId, tree);
		Assert.assertEquals("Hash tree expectation failure, user " + userId, tree, cache.get(userId));
	}

	@Test
	public void test_can_clear()
	{
		int userId = buildUserId();

		CachedHashTree cache = new CachedHashTree();
		cache.set(userId, buildTree());
		cache.set(userId, null);
		Assert.assertEquals("Hash tree expectation failure, user " + userId, null, cache.get(userId));
	}

	@Test
	public void test_independence_by_instance()
	{
		CachedHashTree cache1 = new CachedHashTree();
		CachedHashTree cache2 = new CachedHashTree();

		MerkleTree tree1 = buildTree();
		MerkleTree tree2 = buildTree();

		int userId = buildUserId();

		// ----------------------------------------

		cache1.set(userId, tree1);
		Assert.assertEquals("Hash tree expectation failure, user " + userId, tree1, cache1.get(userId));
		Assert.assertEquals("Hash tree expectation failure, user " + userId, null, cache2.get(userId));

		cache2.set(userId, tree2);
		Assert.assertEquals("Hash tree expectation failure, user " + userId, tree1, cache1.get(userId));
		Assert.assertEquals("Hash tree expectation failure, user " + userId, tree2, cache2.get(userId));

		cache1.set(userId, null);
		Assert.assertEquals("Hash tree expectation failure, user " + userId, null, cache1.get(userId));
		Assert.assertEquals("Hash tree expectation failure, user " + userId, tree2, cache2.get(userId));
	}

	@Test
	public void test_independence_by_user()
	{
		CachedHashTree cache = new CachedHashTree();

		MerkleTree tree1 = buildTree();
		MerkleTree tree2 = buildTree();

		int userId1 = buildUserId() % 100000;
		int userId2 = userId1 + 1;

		// ----------------------------------------

		cache.set(userId1, tree1);
		Assert.assertEquals("Hash tree expectation failure, user " + userId2, null, cache.get(userId2));

		cache.set(userId2, tree2);
		Assert.assertEquals("Hash tree expectation failure, user " + userId1, tree1, cache.get(userId1));

		cache.set(userId2, null);
		Assert.assertEquals("Hash tree expectation failure, user " + userId1, tree1, cache.get(userId1));
	}
}
