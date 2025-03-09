/*
 * MerkleSync - Data synchronization routine based on Merkle hash trees
 * Copyright (C) 2013 Nikita Bosik
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.bosik.merklesync;

import jdk.nashorn.internal.ir.debug.ObjectSizeCalculator;
import org.bosik.diacomp.core.utils.Profiler;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

@SuppressWarnings("static-method")
public class TestMemoryMerkleTree
{
	private static final Map<String, String>       MAX_EXAMPLE       = buildCompleteExample();
	private static final SortedMap<String, String> HASH_TREE         = HashUtils.buildHashTree(new TreeMap<>(MAX_EXAMPLE), DataSource.ID_PREFIX_SIZE);
	private static final Set<String>               ALL_KEYS          = buildKeys(DataSource.ID_PREFIX_SIZE);
	private static final Set<String>               SHORT_KEYS        = buildKeys(DataSource.ID_PREFIX_SIZE - 1);
	private static final MerkleTree                MAX_MERKLE_TREE   = new MemoryMerkleTree(HASH_TREE);
	private static final MerkleTree                MAX_MERKLE_TREE_2 = new MemoryMerkleTree2(HASH_TREE);
	private static final MerkleTree                MAX_MERKLE_TREE_3 = new MemoryMerkleTree3(HASH_TREE);

	private static Map<String, String> buildCompleteExample()
	{
		Map<String, String> map = new HashMap<>();

		int max = 1 << 4 * DataSource.ID_PREFIX_SIZE;
		for (int j = 0; j < max; j++)
		{
			final String key = formatHex(j, DataSource.ID_PREFIX_SIZE) + HashUtils.generateGuid().substring(DataSource.ID_PREFIX_SIZE);
			map.put(key, HashUtils.generateGuid());
		}

		return map;
	}

	private static Set<String> buildKeys(int maxSize)
	{
		Set<String> keys = new HashSet<>();

		for (int i = 0; i <= maxSize; i++)
		{
			int max = 1 << 4 * i;
			for (int j = 0; j < max; j++)
			{
				keys.add(formatHex(j, i));
			}
		}

		return keys;
	}

	private static String formatHex(int value, int width)
	{
		if (width == 0)
		{
			return "";
		}

		StringBuilder key = new StringBuilder(Integer.toHexString(value).toLowerCase());

		while (key.length() < width)
		{
			key.insert(0, '0');
		}

		return key.toString();
	}

	@Test
	public void test_getHash_empty()
	{
		MerkleTree m1 = new MemoryMerkleTree(new HashMap<>());
		MerkleTree m2 = new MemoryMerkleTree2(new HashMap<>());
		MerkleTree m3 = new MemoryMerkleTree3(new HashMap<>());

		for (String key : ALL_KEYS)
		{
			Assert.assertNull(m1.getHash(key));
			Assert.assertNull(m2.getHash(key));
			Assert.assertNull(m3.getHash(key));
		}
	}

	@Test
	public void test_getHash()
	{
		for (Map.Entry<String, String> entry : HASH_TREE.entrySet())
		{
			Assert.assertEquals(entry.getValue(), MAX_MERKLE_TREE.getHash(entry.getKey()));
			Assert.assertEquals(entry.getValue(), MAX_MERKLE_TREE_2.getHash(entry.getKey()));
			Assert.assertEquals(entry.getValue(), MAX_MERKLE_TREE_3.getHash(entry.getKey()));
		}
	}

	@Test
	public void test_getHashChildren_empty()
	{
		MerkleTree m1 = new MemoryMerkleTree(new HashMap<>());
		MerkleTree m2 = new MemoryMerkleTree2(new HashMap<>());
		MerkleTree m3 = new MemoryMerkleTree3(new HashMap<>());

		for (String key : SHORT_KEYS)
		{
			Assert.assertEquals(Collections.emptyMap(), m1.getHashChildren(key));
			Assert.assertEquals(Collections.emptyMap(), m2.getHashChildren(key));
			Assert.assertEquals(Collections.emptyMap(), m3.getHashChildren(key));
		}
	}

	@Test
	public void test_getHashChildren()
	{
		for (String key : SHORT_KEYS)
		{
			Map<String, String> children1 = MAX_MERKLE_TREE.getHashChildren(key);
			Map<String, String> children2 = MAX_MERKLE_TREE_2.getHashChildren(key);
			Map<String, String> children3 = MAX_MERKLE_TREE_3.getHashChildren(key);

			for (int k = 0; k < 16; k++)
			{
				final String subKey = key + Integer.toHexString(k).toLowerCase();
				final String subValue = HASH_TREE.get(subKey);

				Assert.assertEquals(subValue, children1.get(subKey));
				Assert.assertEquals(subValue, children2.get(subKey));
				Assert.assertEquals(subValue, children3.get(subKey));
			}
		}
	}

	@Test
	@Ignore("This test case is for manual performance check only")
	public void test_performance_memory()
	{
		//		MemoryMerkleTree         	  13 386 880 (bytes)
		//		MemoryMerkleTree2        	   7 549 880 (bytes)
		//		MemoryMerkleTree3        	   2 516 720 (bytes)

		String[] arrayString = new String[HASH_TREE.size()];
		for (int i = 0; i < arrayString.length; i++)
		{
			arrayString[i] = HashUtils.generateGuid();
		}

		byte[][] arrayByte = new byte[HASH_TREE.size()][16];
		final Random random = new Random();

		for (byte[] anArrayByte : arrayByte)
		{
			for (int j = 0; j < 16; j++)
			{
				random.nextBytes(anArrayByte);
			}
		}

		System.out.printf("%-25s\t%10s%n", "OBJECT", "SIZE (bytes)");
		System.out.printf("%-25s\t%,10d%n", "HashMap", getObjectSize(HASH_TREE));
		System.out.printf("%-25s\t%,10d%n", MemoryMerkleTree.class.getSimpleName(), getObjectSize(MAX_MERKLE_TREE));
		System.out.printf("%-25s\t%,10d%n", MemoryMerkleTree2.class.getSimpleName(), getObjectSize(MAX_MERKLE_TREE_2));
		System.out.printf("%-25s\t%,10d%n", MemoryMerkleTree3.class.getSimpleName(), getObjectSize(MAX_MERKLE_TREE_3));
		System.out.printf("%-25s\t%,10d%n", "String array", getObjectSize(arrayString));
		System.out.printf("%-25s\t%,10d%n", "Byte array", getObjectSize(arrayByte));
	}

	private long getObjectSize(Object object)
	{
		return ObjectSizeCalculator.getObjectSize(object);
	}

	@Test
	@Ignore("This test case is for manual performance check only")
	public void test_performance_getHash()
	{
		final int COUNT = 800;

		//		MemoryMerkleTree: 16.928 ms (TreeMap)
		//		MemoryMerkleTree: 3.586 ms (HashMap)
		//		MemoryMerkleTree2: 2.649 ms
		//		MemoryMerkleTree3: 7.124 ms

		System.out.printf(Locale.US, MemoryMerkleTree.class.getSimpleName() + ": %.3f ms%n", Profiler.measureInMsec(() -> {
			for (String key : HASH_TREE.keySet())
			{
				MAX_MERKLE_TREE.getHash(key);
			}
		}, COUNT));

		System.out.printf(Locale.US, MemoryMerkleTree2.class.getSimpleName() + ": %.3f ms%n", Profiler.measureInMsec(() -> {
			for (String key : HASH_TREE.keySet())
			{
				MAX_MERKLE_TREE_2.getHash(key);
			}
		}, COUNT));

		System.out.printf(Locale.US, MemoryMerkleTree3.class.getSimpleName() + ": %.3f ms%n", Profiler.measureInMsec(() -> {
			for (String key : HASH_TREE.keySet())
			{
				MAX_MERKLE_TREE_3.getHash(key);
			}
		}, COUNT));
	}

	@Test
	@Ignore("This test case is for manual performance check only")
	public void test_performance_getHashChildren()
	{
		final int COUNT = 500;

		//		MemoryMerkleTree: 10.846 ms (HashMap)
		//		MemoryMerkleTree2: 3.097 ms
		//		MemoryMerkleTree3: 6.763 ms

		System.out.printf(Locale.US, MemoryMerkleTree.class.getSimpleName() + ": %.3f ms%n", Profiler.measureInMsec(() -> {
			for (String key : SHORT_KEYS)
			{
				MAX_MERKLE_TREE.getHashChildren(key);
			}
		}, COUNT));

		System.out.printf(Locale.US, MemoryMerkleTree2.class.getSimpleName() + ": %.3f ms%n", Profiler.measureInMsec(() -> {
			for (String key : SHORT_KEYS)
			{
				MAX_MERKLE_TREE_2.getHashChildren(key);
			}
		}, COUNT));

		System.out.printf(Locale.US, MemoryMerkleTree3.class.getSimpleName() + ": %.3f ms%n", Profiler.measureInMsec(() -> {
			for (String key : SHORT_KEYS)
			{
				MAX_MERKLE_TREE_3.getHashChildren(key);
			}
		}, COUNT));
	}
}
