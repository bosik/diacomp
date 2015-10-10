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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map.Entry;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.UUID;

public class HashUtils
{
	public static final int		PATTERN_SIZE	= 16;
	public static final char[]	BYTE_TO_CHAR	= new char[] { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a',
			'b', 'c', 'd', 'e', 'f'			};
	public static final byte[]	CHAR_TO_BYTE	= new byte[65536];

	static
	{
		for (int i = 0; i < PATTERN_SIZE; i++)
		{
			char c_lower = BYTE_TO_CHAR[i];
			char c_upper = ("" + c_lower).toUpperCase().charAt(0);

			CHAR_TO_BYTE[c_lower] = (byte)i;
			CHAR_TO_BYTE[c_upper] = (byte)i;
		}
	}

	/**
	 * null + a = a<br/>
	 * null + null = null
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static String sumHash(String a, String b)
	{
		if (a != null && a.length() != DataSource.ID_FULL_SIZE)
		{
			throw new IllegalArgumentException(String.format("Invalid hash #1 ('%s'), expected: %d chars, found: %d",
					a, DataSource.ID_FULL_SIZE, a.length()));
		}

		if (b != null && b.length() != DataSource.ID_FULL_SIZE)
		{
			throw new IllegalArgumentException(String.format("Invalid hash #2 ('%s'), expected: %d chars, found: %d",
					b, DataSource.ID_FULL_SIZE, b.length()));
		}

		if (a == null)
		{
			return b;
		}

		if (b == null)
		{
			return a;
		}

		char[] a_array = a.toCharArray();
		char[] b_array = b.toCharArray();
		char[] c_array = new char[DataSource.ID_FULL_SIZE];

		for (int i = 0; i < DataSource.ID_FULL_SIZE; i++)
		{
			byte b1 = CHAR_TO_BYTE[a_array[i]];
			byte b2 = CHAR_TO_BYTE[b_array[i]];
			c_array[i] = BYTE_TO_CHAR[(b1 + b2) % 16];
		}

		return new String(c_array);
	}

	public static String calculateHash(Collection<String> collection)
	{
		String result = null;

		for (String hash : collection)
		{
			result = sumHash(result, hash);
		}

		return result;
	}

	public static SortedMap<String, String> buildParentHashes(SortedMap<String, String> map, int prefixSize)
	{
		SortedMap<String, String> result = new TreeMap<String, String>();

		List<String> buffer = new ArrayList<String>();
		String prevKey = null;
		for (Entry<String, String> entry : map.entrySet())
		{
			if (entry.getKey().length() < prefixSize)
			{
				throw new IllegalArgumentException(String.format("Invalid key '%s', must be at least %d chars long",
						entry.getKey(), prefixSize));
			}

			if (prevKey != null && !prevKey.regionMatches(0, entry.getKey(), 0, prefixSize))
			{
				String key = prevKey.substring(0, prefixSize);
				String value = calculateHash(buffer);
				result.put(key, value);
				buffer.clear();
			}
			buffer.add(entry.getValue());
			prevKey = entry.getKey();
		}

		if (!buffer.isEmpty())
		{
			String key = prevKey.substring(0, prefixSize);
			String value = calculateHash(buffer);
			result.put(key, value);
		}

		return result;
	}

	public static SortedMap<String, String> buildHashTree(SortedMap<String, String> map)
	{
		// Validation
		for (String key : map.keySet())
		{
			if (key.length() < DataSource.ID_PREFIX_SIZE)
			{
				throw new IllegalArgumentException(String.format("Invalid key '%s', must be at least %d chars long",
						key, DataSource.ID_PREFIX_SIZE));
			}
		}

		// Process
		SortedMap<String, String> result = new TreeMap<String, String>();

		for (int i = DataSource.ID_PREFIX_SIZE; i >= 0; i--)
		{
			map = buildParentHashes(map, i);
			result.putAll(map);
		}

		return result;
	}

	/**
	 * Generates pseudo-random 32-chars-long GUID
	 * 
	 * @return
	 */
	public static String generateGuid()
	{
		return UUID.randomUUID().toString().replace("-", "").toLowerCase();
	}
}
