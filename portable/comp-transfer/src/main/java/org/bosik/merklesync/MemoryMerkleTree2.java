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

import java.util.HashMap;
import java.util.Map;

/**
 * String-array-based Merkle tree. Immutable.
 */
public class MemoryMerkleTree2 implements MerkleTree
{
	private final String[][] table;

	public MemoryMerkleTree2(Map<String, String> map)
	{
		table = new String[DataSource.ID_PREFIX_SIZE + 1][];
		for (int i = 0; i <= DataSource.ID_PREFIX_SIZE; i++)
		{
			table[i] = new String[1 << 4 * i];
		}

		for (Map.Entry<String, String> entry : map.entrySet())
		{
			String key = entry.getKey();
			if (key.length() <= DataSource.ID_PREFIX_SIZE)
			{
				table[key.length()][HashUtils.toInt(key)] = entry.getValue();
			}
			else
			{
				throw new IllegalArgumentException(
						"Too long key '" + entry.getKey() + "': max " + DataSource.ID_PREFIX_SIZE + " chars allowed");
			}
		}
	}

	@Override
	public String getHash(String prefix)
	{
		if (prefix.length() <= DataSource.ID_PREFIX_SIZE)
		{
			return table[prefix.length()][HashUtils.toInt(prefix)];
		}
		else
		{
			throw new IllegalArgumentException("Too long key '" + prefix + "': max " + DataSource.ID_PREFIX_SIZE + " chars allowed");
		}
	}

	@Override
	public Map<String, String> getHashChildren(final String prefix)
	{
		if (prefix.length() < DataSource.ID_PREFIX_SIZE)
		{
			Map<String, String> directChildren = new HashMap<>();

			int from = HashUtils.toInt(prefix) << 4;

			for (int i = 0; i < 16; i++)
			{
				String value = table[prefix.length() + 1][from + i];
				if (value != null)
				{
					directChildren.put(prefix + HashUtils.byteToChar(i), value);
				}
			}

			return directChildren;
		}
		else
		{
			throw new IllegalArgumentException("Too long key '" + prefix + "': max " + (DataSource.ID_PREFIX_SIZE - 1) + " chars allowed");
		}
	}
}
