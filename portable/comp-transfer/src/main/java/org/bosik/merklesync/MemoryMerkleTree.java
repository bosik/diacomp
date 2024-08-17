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
 * Deprecated. Use {@link MemoryMerkleTree2} instead.<br/>
 * TreeMap-based Merkle tree.
 */
@Deprecated
public class MemoryMerkleTree implements MerkleTree
{
	private static final long serialVersionUID = 4901379605259573068L;

	private final Map<String, String> map = new HashMap<>();

	public MemoryMerkleTree()
	{

	}

	public MemoryMerkleTree(Map<String, String> map)
	{
		this.map.putAll(map);
	}

	@Override
	public String getHash(String prefix)
	{
		return map.get(prefix);
	}

	@Override
	public Map<String, String> getHashChildren(final String prefix)
	{
		if (prefix.length() < DataSource.ID_PREFIX_SIZE)
		{
			Map<String, String> directChildren = new HashMap<>();

			for (int i = 0; i < 16; i++)
			{
				String key = prefix + HashUtils.byteToChar(i);
				String value = map.get(key);
				if (value != null)
				{
					directChildren.put(key, value);
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
