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
import java.util.Locale;
import java.util.Map.Entry;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.UUID;

public class HashUtils
{
	private static final char[] BYTE_TO_CHAR = new char[] { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };
	private static final byte[] CHAR_TO_BYTE = new byte[65536];

	static
	{
		for (byte i = 0; i < BYTE_TO_CHAR.length; i++)
		{
			char c_lower = BYTE_TO_CHAR[i];
			char c_upper = ("" + c_lower).toUpperCase().charAt(0);

			CHAR_TO_BYTE[c_lower] = i;
			CHAR_TO_BYTE[c_upper] = i;
		}
	}

	private static final char[] BYTE_HIGH = new char[256];
	private static final char[] BYTE_LOW  = new char[256];

	static
	{
		for (int i = 0; i < 256; i++)
		{
			BYTE_HIGH[i] = BYTE_TO_CHAR[i >> 4];
			BYTE_LOW[i] = BYTE_TO_CHAR[i % 16];
		}
	}

	// --------------------------------------------------------------------------------------
	// CONVERSION
	// --------------------------------------------------------------------------------------

	public static int toInt(String s)
	{
		return toInt(s, s.length());
	}

	public static int toInt(String s, int prefixSize)
	{
		int value = 0;

		for (int i = 0; i < prefixSize; i++)
		{
			value *= 16;
			value += CHAR_TO_BYTE[s.charAt(i)];
		}

		return value;
	}

	public static String toHex(int n, int size)
	{
		String s = "";
		while (s.length() < size)
		{
			s = BYTE_TO_CHAR[n % 16] + s;
			n /= 16;
		}

		return s;
	}

	public static char byteToChar(int x)
	{
		return BYTE_TO_CHAR[x];
	}

	public static byte charToByte(char c)
	{
		return CHAR_TO_BYTE[c];
	}

	public static byte[] strToByte(String value)
	{
		if (value == null)
		{
			return null;
		}

		byte[] result = new byte[value.length() >> 1];
		for (int i = 0; i < result.length; i++)
		{
			final char c1 = value.charAt(i << 1);
			final char c2 = value.charAt((i << 1) + 1);
			result[i] = (byte) ((CHAR_TO_BYTE[c1] << 4) + CHAR_TO_BYTE[c2]);
		}

		return result;
	}

	public static String byteToStr(byte[] bytes)
	{
		if (bytes == null)
		{
			return null;
		}

		char[] chars = new char[bytes.length << 1];
		int i = 0;
		for (int b : bytes)
		{
			int n = (b + 256) % 256;
			chars[i++] = BYTE_HIGH[n];
			chars[i++] = BYTE_LOW[n];
		}

		return new String(chars);
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
		if (a != null && a.length() != DataSource.HASH_SIZE)
		{
			throw new IllegalArgumentException(
					String.format(Locale.US, "Invalid hash #1 ('%s'), expected: %d chars, found: %d", a, DataSource.HASH_SIZE, a.length()));
		}

		if (b != null && b.length() != DataSource.HASH_SIZE)
		{
			throw new IllegalArgumentException(
					String.format(Locale.US, "Invalid hash #2 ('%s'), expected: %d chars, found: %d", b, DataSource.HASH_SIZE, b.length()));
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
		char[] c_array = new char[DataSource.HASH_SIZE];

		for (int i = 0; i < DataSource.HASH_SIZE; i++)
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
		SortedMap<String, String> result = new TreeMap<>();

		List<String> buffer = new ArrayList<>();
		String prevKey = null;
		for (Entry<String, String> entry : map.entrySet())
		{
			if (entry.getKey().length() < prefixSize)
			{
				throw new IllegalArgumentException(
						String.format(Locale.US, "Invalid entry ('%s'='%s'): key must be at least %d chars long", entry.getKey(),
								entry.getValue(), prefixSize));
			}

			if (entry.getValue() == null)
			{
				throw new IllegalArgumentException(
						String.format(Locale.US, "Invalid entry ('%s'='%s'): value is null", entry.getKey(), entry.getValue()));
			}

			if (entry.getValue().length() < DataSource.HASH_SIZE)
			{
				throw new IllegalArgumentException(
						String.format(Locale.US, "Invalid entry ('%s'='%s'): value is too short, at least %d chars expected", entry.getKey(),
								entry.getValue(), DataSource.HASH_SIZE));
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
			@SuppressWarnings("null") String key = prevKey.substring(0, prefixSize);
			String value = calculateHash(buffer);
			result.put(key, value);
		}

		return result;
	}

	public static SortedMap<String, String> buildHashTree(SortedMap<String, String> map)
	{
		SortedMap<String, String> result = new TreeMap<>();

		for (int i = DataSource.ID_PREFIX_SIZE; i >= 0; i--)
		{
			map = buildParentHashes(map, i);
			result.putAll(map);
		}

		return result;
	}

	/**
	 * Create in-memory Merkle tree
	 *
	 * @param hashes Map (id -> hash) for all data items; id's max length is DataSource.ID_PREFIX_SIZE
	 * @return
	 */
	public static MerkleTree buildMerkleTree(SortedMap<String, String> hashes)
	{
		// headers (0..4 chars id)
		return new MemoryMerkleTree3(buildHashTree(hashes));
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
