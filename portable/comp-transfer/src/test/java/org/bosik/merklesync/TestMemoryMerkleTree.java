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
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.SortedMap;
import java.util.TreeMap;

@SuppressWarnings("static-method")
public class TestMemoryMerkleTree
{
	private static final Map<String, String> MAX_MAP           = buildCompleteHashes();
	private static final MerkleTree          MAX_MERKLE_TREE   = new MemoryMerkleTree(MAX_MAP);
	private static final MerkleTree          MAX_MERKLE_TREE_2 = new MemoryMerkleTree2(MAX_MAP);

	private static SortedMap<String, String> buildDataHashes()
	{
		SortedMap<String, String> map = new TreeMap<>();

		for (int i = 0; i < MAX_MAP.size() * 16; i++)
		{
			map.put(HashUtils.generateGuid(), HashUtils.generateGuid());
		}

		return map;
	}

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

	@Test
	@Ignore
	public void test_size()
	{
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

		System.out.println("Map size, bytes:         \t" + ObjectSizeCalculator.getObjectSize(MAX_MAP));
		System.out.println("Tree size (old), bytes:  \t" + ObjectSizeCalculator.getObjectSize(MAX_MERKLE_TREE));
		System.out.println("Tree size (new), bytes:  \t" + ObjectSizeCalculator.getObjectSize(MAX_MERKLE_TREE_2));
		System.out.println("String array size, bytes:\t" + ObjectSizeCalculator.getObjectSize(arrayString));
		System.out.println("Byte array size, bytes:  \t" + ObjectSizeCalculator.getObjectSize(arrayByte));
	}

	@Test
	public void test_getHash()
	{
		for (Map.Entry<String, String> entry : MAX_MAP.entrySet())
		{
			Assert.assertEquals(entry.getValue(), MAX_MERKLE_TREE_2.getHash(entry.getKey()));
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

				Map<String, String> children = MAX_MERKLE_TREE_2.getHashChildren(key);

				for (int k = 0; k < 16; k++)
				{
					String subKey = key + Integer.toHexString(k).toLowerCase();
					String subValue = MAX_MAP.get(subKey);

					Assert.assertEquals(subValue, children.get(subKey));
				}
			}
		}
	}
}
