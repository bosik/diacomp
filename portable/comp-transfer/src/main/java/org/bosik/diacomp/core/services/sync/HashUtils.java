package org.bosik.diacomp.core.services.sync;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map.Entry;
import java.util.SortedMap;
import java.util.TreeMap;
import org.bosik.diacomp.core.services.ObjectService;

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
		if (a != null && a.length() != ObjectService.ID_FULL_SIZE)
		{
			throw new IllegalArgumentException(String.format("Invalid hash #1 ('%s'), expected: %d chars, found: %d",
					a, ObjectService.ID_FULL_SIZE, a.length()));
		}

		if (b != null && b.length() != ObjectService.ID_FULL_SIZE)
		{
			throw new IllegalArgumentException(String.format("Invalid hash #2 ('%s'), expected: %d chars, found: %d",
					b, ObjectService.ID_FULL_SIZE, b.length()));
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
		char[] c_array = new char[ObjectService.ID_FULL_SIZE];

		for (int i = 0; i < ObjectService.ID_FULL_SIZE; i++)
		{
			byte b1 = CHAR_TO_BYTE[a_array[i]];
			byte b2 = CHAR_TO_BYTE[b_array[i]];
			c_array[i] = BYTE_TO_CHAR[(b1 + b2) % 16];
		}

		return new String(c_array);
	}

	/**
	 * a - null = a<br/>
	 * null - null = null<br/>
	 * null - a = NPE
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static String subHash(String a, String b)
	{
		if (a != null && a.length() != ObjectService.ID_FULL_SIZE)
		{
			throw new IllegalArgumentException(String.format("Invalid hash #1 ('%s'), expected: %d chars, found: %d",
					a, ObjectService.ID_FULL_SIZE, a.length()));
		}

		if (b != null && b.length() != ObjectService.ID_FULL_SIZE)
		{
			throw new IllegalArgumentException(String.format("Invalid hash #2 ('%s'), expected: %d chars, found: %d",
					b, ObjectService.ID_FULL_SIZE, b.length()));
		}

		if (b == null)
		{
			return a;
		}

		char[] a_array = a.toCharArray();
		char[] b_array = b.toCharArray();
		char[] c_array = new char[ObjectService.ID_FULL_SIZE];

		for (int i = 0; i < ObjectService.ID_FULL_SIZE; i++)
		{
			byte b1 = CHAR_TO_BYTE[a_array[i]];
			byte b2 = CHAR_TO_BYTE[b_array[i]];
			c_array[i] = BYTE_TO_CHAR[(16 + b1 - b2) % 16];
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
			if (key.length() < ObjectService.ID_PREFIX_SIZE)
			{
				throw new IllegalArgumentException(String.format("Invalid key '%s', must be at least %d chars long",
						key, ObjectService.ID_PREFIX_SIZE));
			}
		}

		// Process
		SortedMap<String, String> result = new TreeMap<String, String>();

		for (int i = ObjectService.ID_PREFIX_SIZE; i >= 0; i--)
		{
			map = buildParentHashes(map, i);
			result.putAll(map);
		}

		return result;
	}
}
