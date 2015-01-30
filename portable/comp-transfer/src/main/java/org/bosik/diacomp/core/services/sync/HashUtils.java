package org.bosik.diacomp.core.services.sync;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.SortedMap;
import java.util.TreeMap;
import org.bosik.diacomp.core.services.ObjectService;

public class HashUtils
{
	/**
	 * Standard alphanumeric hash chars
	 */
	public static final String					PATTERN			= "0123456789abcdef";
	public static final int						PATTERN_SIZE	= PATTERN.length();
	private static final Map<String, Character>	MAP_SUM			= new HashMap<String, Character>(PATTERN_SIZE
																		* PATTERN_SIZE);
	private static final Map<String, Character>	MAP_SUB			= new HashMap<String, Character>(PATTERN_SIZE
																		* PATTERN_SIZE);

	static
	{
		System.out.println("Hash tables initialization...");

		MAP_SUM.clear();
		MAP_SUB.clear();

		for (int i = 0; i < PATTERN_SIZE; i++)
		{
			char a = PATTERN.charAt(i);
			for (int j = 0; j < PATTERN_SIZE; j++)
			{
				char b = PATTERN.charAt(j);
				String key = "" + a + b;
				MAP_SUM.put(key, PATTERN.charAt((i + j) % PATTERN_SIZE));
				MAP_SUB.put(key, PATTERN.charAt((i - j + PATTERN_SIZE) % PATTERN_SIZE));
			}
		}

		System.out.println("Hash tables initialized OK");
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

		a = a.toLowerCase();
		b = b.toLowerCase();

		StringBuilder result = new StringBuilder(ObjectService.ID_FULL_SIZE);
		for (int i = 0; i < ObjectService.ID_FULL_SIZE; i++)
		{
			char ca = a.charAt(i);
			char cb = b.charAt(i);
			result.append(MAP_SUM.get("" + ca + cb));
		}

		return result.toString();
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

		a = a.toLowerCase();
		b = b.toLowerCase();

		StringBuilder result = new StringBuilder(ObjectService.ID_FULL_SIZE);
		for (int i = 0; i < ObjectService.ID_FULL_SIZE; i++)
		{
			char ca = a.charAt(i);
			char cb = b.charAt(i);
			result.append(MAP_SUB.get("" + ca + cb));
		}

		return result.toString();
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

	/**
	 * Updates tree from leaf to root. Fast, not stable.
	 * 
	 * @param service
	 * @param prefix
	 */
	public static <T> void updateHashBranch(ObjectService<T> service, String prefix)
	{
		Map<String, String> hashes = service.getHashChildren(prefix);
		String hash = calculateHash(hashes.values());
		if (hash != null)
		{
			service.setHash(prefix, hash);
		}
		if (prefix.length() > 0)
		{
			updateHashBranch(service, prefix.substring(0, prefix.length() - 1));
		}
	}

	/**
	 * Updates tree from root to leafs. Slow, very stable.
	 * 
	 * @param service
	 * @param prefix
	 * @return
	 */
	public static <T> String updateHashTree(ObjectService<T> service, String prefix)
	{
		if (prefix.length() < 3)
		{
			System.out.println("Updating tree branch # " + prefix);
		}

		Map<String, String> childHashes;

		if (prefix.length() < ObjectService.ID_PREFIX_SIZE)
		{
			childHashes = new HashMap<String, String>();
			for (int i = 0; i < PATTERN_SIZE; i++)
			{
				String key = prefix + PATTERN.charAt(i);
				String value = updateHashTree(service, key);

				if (value != null)
				{
					childHashes.put(key, value);
				}
			}
		}
		else
		{
			childHashes = service.getHashChildren(prefix);
		}

		String hash = calculateHash(childHashes.values());
		if (hash != null)
		{
			service.setHash(prefix, hash);
		}
		return hash;
	}

	public static SortedMap<String, String> buildParentHashes(SortedMap<String, String> map, int prefixSize)
	{
		SortedMap<String, String> result = new TreeMap<String, String>();

		List<String> buffer = new ArrayList<String>();
		String prevKey = null;
		for (Entry<String, String> entry : map.entrySet())
		{
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
		SortedMap<String, String> result = new TreeMap<String, String>();

		for (int i = ObjectService.ID_PREFIX_SIZE; i >= 0; i--)
		{
			map = buildParentHashes(map, i);
			result.putAll(map);
		}

		return result;
	}
}
