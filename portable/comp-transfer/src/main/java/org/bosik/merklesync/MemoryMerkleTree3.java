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
import java.util.Objects;

/**
 * Byte-array-based Merkle tree.
 */
public class MemoryMerkleTree3 implements MerkleTree
{
	private final byte[][][] table;

	public MemoryMerkleTree3(Map<String, String> map)
	{
		table = new byte[DataSource.ID_PREFIX_SIZE + 1][][];
		for (int i = 0; i <= DataSource.ID_PREFIX_SIZE; i++)
		{
			table[i] = new byte[1 << 4 * i][];
		}

		for (Map.Entry<String, String> entry : map.entrySet())
		{
			String key = entry.getKey();
			if (key.length() <= DataSource.ID_PREFIX_SIZE)
			{
				table[key.length()][HashUtils.toInt(key)] = HashUtils.strToByte(entry.getValue());
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
			byte[] bytes = table[prefix.length()][HashUtils.toInt(prefix)];
			return HashUtils.byteToStr(bytes);
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
				byte[] value = table[prefix.length() + 1][from + i];
				if (value != null)
				{
					directChildren.put(prefix + HashUtils.byteToChar(i), HashUtils.byteToStr(value));
				}
			}

			return directChildren;

			// -------------------------------------------------------

			//			String[] result = new String[16];
			//			int from = HashUtils.toInt(prefix) << 4;
			//			for (int i = 0; i < 16; i++)
			//			{
			//				result[i] = HashUtils.byteToStr(table[prefix.length() + 1][from + i]);
			//			}
			//			return null;
		}
		else
		{
			throw new IllegalArgumentException("Too long key '" + prefix + "': max " + (DataSource.ID_PREFIX_SIZE - 1) + " chars allowed");
		}
	}

	@Override
	public void update(String id, String oldHash, String newHash)
	{
		if (Objects.equals(oldHash, newHash))
		{
			return;
		}

		final byte[] bytesOld = HashUtils.strToByte(oldHash);
		final byte[] bytesNew = HashUtils.strToByte(newHash);
		final byte[] delta = HashUtils.sub(bytesNew, bytesOld);

		for (int prefix = 0; prefix <= DataSource.ID_PREFIX_SIZE; prefix++)
		{
			final int key = HashUtils.toInt(id, prefix);
			table[prefix][key] = HashUtils.add(table[prefix][key], delta);
		}
	}
}
