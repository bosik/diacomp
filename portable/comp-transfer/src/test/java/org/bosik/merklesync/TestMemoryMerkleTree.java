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

@SuppressWarnings("static-method")
public class TestMemoryMerkleTree
{
	private static final Map<String, String> MAX_MAP           = buildCompleteHashes();
	private static final Set<String>         SHORT_KEYS        = buildShortKeys(MAX_MAP.keySet());
	private static final MerkleTree          MAX_MERKLE_TREE   = new MemoryMerkleTree(MAX_MAP);
	private static final MerkleTree          MAX_MERKLE_TREE_2 = new MemoryMerkleTree2(MAX_MAP);
	private static final MerkleTree          MAX_MERKLE_TREE_3 = new MemoryMerkleTree3(MAX_MAP);

	private static Map<String, String> buildCompleteHashes()
	{
		Map<String, String> map = new HashMap<>();

		map.put("", HashUtils.generateGuid());

		for (int i = 1; i <= DataSource.ID_PREFIX_SIZE; i++)
		{
			int max = 1 << 4 * i;
			for (int j = 0; j < max; j++)
			{
				String key = Integer.toHexString(j).toLowerCase();
				while (key.length() < i)
				{
					key = '0' + key;
				}

				map.put(key, HashUtils.generateGuid());
			}
		}

		return map;
	}

	private static Set<String> buildShortKeys(Set<String> allKeys)
	{
		Set<String> keys = new HashSet<>();

		for (String key : allKeys)
		{
			if (key.length() < DataSource.ID_PREFIX_SIZE)
			{
				keys.add(key);
			}
		}

		return keys;
	}

	@Test
	public void test_getHash_empty()
	{
		MerkleTree m1 = new MemoryMerkleTree(new HashMap<String, String>());
		MerkleTree m2 = new MemoryMerkleTree2(new HashMap<String, String>());
		MerkleTree m3 = new MemoryMerkleTree3(new HashMap<String, String>());

		for (Map.Entry<String, String> entry : MAX_MAP.entrySet())
		{
			Assert.assertEquals(null, m1.getHash(entry.getKey()));
			Assert.assertEquals(null, m2.getHash(entry.getKey()));
			Assert.assertEquals(null, m3.getHash(entry.getKey()));
		}
	}

	@Test
	public void test_getHash()
	{
		for (Map.Entry<String, String> entry : MAX_MAP.entrySet())
		{
			Assert.assertEquals(entry.getValue(), MAX_MERKLE_TREE.getHash(entry.getKey()));
			Assert.assertEquals(entry.getValue(), MAX_MERKLE_TREE_2.getHash(entry.getKey()));
			Assert.assertEquals(entry.getValue(), MAX_MERKLE_TREE_3.getHash(entry.getKey()));
		}
	}

	@Test
	public void test_getHashChildren_empty()
	{
		MerkleTree m1 = new MemoryMerkleTree(new HashMap<String, String>());
		MerkleTree m2 = new MemoryMerkleTree2(new HashMap<String, String>());
		MerkleTree m3 = new MemoryMerkleTree3(new HashMap<String, String>());

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
		for (int i = 0; i < DataSource.ID_PREFIX_SIZE; i++)
		{
			int max = 1 << 4 * i;
			for (int j = 0; j < max; j++)
			{
				String key = (i > 0) ? Integer.toHexString(j).toLowerCase() : "";
				while (key.length() < i)
				{
					key = '0' + key;
				}

				Map<String, String> children1 = MAX_MERKLE_TREE.getHashChildren(key);
				Map<String, String> children2 = MAX_MERKLE_TREE_2.getHashChildren(key);
				Map<String, String> children3 = MAX_MERKLE_TREE_3.getHashChildren(key);

				for (int k = 0; k < 16; k++)
				{
					String subKey = key + Integer.toHexString(k).toLowerCase();
					String subValue = MAX_MAP.get(subKey);

					Assert.assertEquals(subValue, children1.get(subKey));
					Assert.assertEquals(subValue, children2.get(subKey));
					Assert.assertEquals(subValue, children3.get(subKey));
				}
			}
		}
	}

	@Test
	@Ignore
	public void test_memory()
	{
		//		MemoryMerkleTree         	  13 386 880 (bytes)
		//		MemoryMerkleTree2        	   7 549 880 (bytes)
		//		MemoryMerkleTree3        	   2 516 720 (bytes)

		String[] arrayString = new String[MAX_MAP.size()];
		for (int i = 0; i < arrayString.length; i++)
		{
			arrayString[i] = HashUtils.generateGuid();
		}

		byte[][] arrayByte = new byte[MAX_MAP.size()][16];
		for (byte[] anArrayByte : arrayByte)
		{
			for (int j = 0; j < 16; j++)
			{
				new Random().nextBytes(anArrayByte);
			}
		}

		System.out.println("SIZE (byes)");
		System.out.printf("%-25s\t%10d%n", "HashMap", ObjectSizeCalculator.getObjectSize(MAX_MAP));
		System.out.printf("%-25s\t%10d%n", MemoryMerkleTree.class.getSimpleName(), ObjectSizeCalculator.getObjectSize(MAX_MERKLE_TREE));
		System.out.printf("%-25s\t%10d%n", MemoryMerkleTree2.class.getSimpleName(), ObjectSizeCalculator.getObjectSize(MAX_MERKLE_TREE_2));
		System.out.printf("%-25s\t%10d%n", MemoryMerkleTree3.class.getSimpleName(), ObjectSizeCalculator.getObjectSize(MAX_MERKLE_TREE_3));
		System.out.printf("%-25s\t%10d%n", "String array", ObjectSizeCalculator.getObjectSize(arrayString));
		System.out.printf("%-25s\t%10d%n", "Byte array", ObjectSizeCalculator.getObjectSize(arrayByte));
	}

	@Test
	@Ignore
	public void test_speed_getHash()
	{
		final int COUNT = 800;

		//		MemoryMerkleTree: 16.928 ms (TreeMap)
		//		MemoryMerkleTree: 3.586 ms (HashMap)
		//		MemoryMerkleTree2: 2.649 ms
		//		MemoryMerkleTree3: 7.124 ms

		System.out.printf(Locale.US, MemoryMerkleTree.class.getSimpleName() + ": %.3f ms%n", Profiler.measureInMsec(new Runnable()
		{
			@Override
			public void run()
			{
				for (String key : MAX_MAP.keySet())
				{
					MAX_MERKLE_TREE.getHash(key);
				}
			}
		}, COUNT));

		System.out.printf(Locale.US, MemoryMerkleTree2.class.getSimpleName() + ": %.3f ms%n", Profiler.measureInMsec(new Runnable()
		{
			@Override
			public void run()
			{
				for (String key : MAX_MAP.keySet())
				{
					MAX_MERKLE_TREE_2.getHash(key);
				}
			}
		}, COUNT));

		System.out.printf(Locale.US, MemoryMerkleTree3.class.getSimpleName() + ": %.3f ms%n", Profiler.measureInMsec(new Runnable()
		{
			@Override
			public void run()
			{
				for (String key : MAX_MAP.keySet())
				{
					MAX_MERKLE_TREE_3.getHash(key);
				}
			}
		}, COUNT));
	}

	@Test
	@Ignore
	public void test_speed_getHashChildren()
	{
		final int COUNT = 500;

		//		MemoryMerkleTree: 10.846 ms (HashMap)
		//		MemoryMerkleTree2: 3.097 ms
		//		MemoryMerkleTree3: 6.763 ms

		System.out.printf(Locale.US, MemoryMerkleTree.class.getSimpleName() + ": %.3f ms%n", Profiler.measureInMsec(new Runnable()
		{
			@Override
			public void run()
			{
				for (String key : SHORT_KEYS)
				{
					MAX_MERKLE_TREE.getHashChildren(key);
				}
			}
		}, COUNT));

		System.out.printf(Locale.US, MemoryMerkleTree2.class.getSimpleName() + ": %.3f ms%n", Profiler.measureInMsec(new Runnable()
		{
			@Override
			public void run()
			{
				for (String key : SHORT_KEYS)
				{
					MAX_MERKLE_TREE_2.getHashChildren(key);
				}
			}
		}, COUNT));

		System.out.printf(Locale.US, MemoryMerkleTree3.class.getSimpleName() + ": %.3f ms%n", Profiler.measureInMsec(new Runnable()
		{
			@Override
			public void run()
			{
				for (String key : SHORT_KEYS)
				{
					MAX_MERKLE_TREE_3.getHashChildren(key);
				}
			}
		}, COUNT));
	}
}
