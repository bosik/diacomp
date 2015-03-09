package org.bosik.diacomp.core.services.sync;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
import org.bosik.diacomp.core.services.preferences.PreferencesService;
import org.bosik.diacomp.core.utils.Utils;

public class SyncUtils
{
	/* ============================ HELPER CLASSES ============================ */

	public static interface Callback
	{
		public void update_max(int max);

		public void update_progress(int progress);
	}

	public static interface ProgressCallback
	{
		public void update(int progress, int max);
	}

	/* ============================ ROUTINES ============================ */

	/**
	 * Calculates lists for synchronization
	 * 
	 * @param items1
	 *            First list
	 * @param items2
	 *            Second list
	 * @param newer1
	 *            Items which has greater version in the first list
	 * @param newer2
	 *            Items which has greater version in the second list
	 * @param only1
	 *            Items which are presented only in the first list
	 * @param only2
	 *            Items which are presented only in the second list
	 */
	private static <T> void getOverLists(List<Versioned<T>> items1, List<Versioned<T>> items2,
			List<Versioned<T>> newer1, List<Versioned<T>> newer2, List<Versioned<T>> only1, List<Versioned<T>> only2)
	{
		// null checks
		if (null == items1)
		{
			throw new IllegalArgumentException("items1 is null");
		}
		if (null == items2)
		{
			throw new IllegalArgumentException("items2 is null");
		}
		if (null == newer1)
		{
			throw new IllegalArgumentException("newer1 is null");
		}
		if (null == newer2)
		{
			throw new IllegalArgumentException("newer2 is null");
		}
		if (null == only1)
		{
			throw new IllegalArgumentException("only1 is null");
		}
		if (null == only2)
		{
			throw new IllegalArgumentException("only2 is null");
		}

		// preparation

		Collections.sort(items1, Versioned.COMPARATOR_GUID);
		Collections.sort(items2, Versioned.COMPARATOR_GUID);
		newer1.clear();
		newer2.clear();
		only1.clear();
		only2.clear();
		int i = 0;
		int j = 0;

		// parallel processing
		while ((i < items1.size()) && (j < items2.size()))
		{
			Versioned<T> p1 = items1.get(i);
			Versioned<T> p2 = items2.get(j);
			int c = Versioned.COMPARATOR_GUID.compare(p1, p2);
			if (c < 0)
			{
				only1.add(p1);
				i++;
			}
			else if (c > 0)
			{
				only2.add(p2);
				j++;
			}
			else
			{
				if (p1.getVersion() > p2.getVersion())
				{
					newer1.add(p1);
				}
				else if (p1.getVersion() < p2.getVersion())
				{
					newer2.add(p2);
				}
				i++;
				j++;
			}
		}

		// finish first list
		while (i < items1.size())
		{
			only1.add(items1.get(i));
			i++;
		}

		// finish second list
		while (j < items2.size())
		{
			only2.add(items2.get(j));
			j++;
		}
	}

	private static <T> int processItems(ObjectService<T> service1, ObjectService<T> service2,
			List<Versioned<T>> items1, List<Versioned<T>> items2)
	{
		// null checks again
		if (null == items1)
		{
			throw new IllegalArgumentException("Service1 returned null list");
		}
		if (null == items2)
		{
			throw new IllegalArgumentException("Service2 returned null list");
		}

		// calculating transferring lists
		List<Versioned<T>> newer1 = new ArrayList<Versioned<T>>();
		List<Versioned<T>> newer2 = new ArrayList<Versioned<T>>();
		List<Versioned<T>> only1 = new ArrayList<Versioned<T>>();
		List<Versioned<T>> only2 = new ArrayList<Versioned<T>>();
		getOverLists(items1, items2, newer1, newer2, only1, only2);

		newer1.addAll(only1);
		newer2.addAll(only2);

		// transfer

		// THINK: divide into small groups?
		service1.save(newer2);
		service2.save(newer1);

		// Result is number of transferred records
		return newer1.size() + newer2.size();
	}

	/* ============================ SYNC METHODS: NAIVE ============================ */

	/**
	 * Synchronizes two object services
	 * 
	 * @param <T>
	 * 
	 * @param service1
	 *            First service
	 * @param service2
	 *            Second service
	 * @param since
	 *            Modification time limiter: items modified after this time is taking in account
	 *            only
	 * @return Total number of transferred items
	 */
	public static <T> int synchronize(ObjectService<T> service1, ObjectService<T> service2, Date since)
	{
		// null checks
		if (null == service1)
		{
			throw new IllegalArgumentException("service1 is null");
		}
		if (null == service2)
		{
			throw new IllegalArgumentException("service2 is null");
		}
		if (null == since)
		{
			throw new IllegalArgumentException("since date is null");
		}

		List<Versioned<T>> items1 = service1.findChanged(since);
		List<Versioned<T>> items2 = service2.findChanged(since);

		return processItems(service1, service2, items1, items2);
	}

	/**
	 * Synchronizes single item
	 * 
	 * @param service1
	 * @param service2
	 * @param id
	 * @return
	 */
	@SuppressWarnings({ "null", "unchecked" })
	public static <T> int synchronize(ObjectService<T> service1, ObjectService<T> service2, String id)
	{
		// null checks
		if (null == service1)
		{
			throw new IllegalArgumentException("service1 is null");
		}
		if (null == service2)
		{
			throw new IllegalArgumentException("service2 is null");
		}
		if (null == id)
		{
			throw new IllegalArgumentException("id is null");
		}

		// requesting items
		Versioned<T> item1 = service1.findById(id);
		Versioned<T> item2 = service2.findById(id);

		if ((item1 == null) && (item2 == null))
		{
			return 0; // item was not found in any sources
		}

		if ((item1 != null) && (item2 == null))
		{
			service2.save(Arrays.asList(item1));
			return 1;
		}

		if ((item1 == null) && (item2 != null))
		{
			service1.save(Arrays.asList(item2));
			return 1;
		}

		if (item1.getVersion() < item2.getVersion())
		{
			service1.save(Arrays.asList(item2));
			return 1;
		}

		if (item1.getVersion() > item2.getVersion())
		{
			service2.save(Arrays.asList(item1));
			return 1;
		}

		return 0;
	}

	/* ============================ SYNC METHODS: HASH-BASED (v1) ============================ */

	private static <T> int synchronizePrefix(ObjectService<T> service1, ObjectService<T> service2, String prefix)
	{
		String hash1 = service1.getHash(prefix);
		String hash2 = service2.getHash(prefix);

		System.out.println("Hashes for " + prefix + ": " + hash1 + " " + hash2);

		if (Utils.equals(hash1, hash2))
		{
			return 0;
		}

		if (prefix.length() < ObjectService.ID_PREFIX_SIZE)
		{
			int result = 0;

			for (int i = 0; i < HashUtils.PATTERN_SIZE; i++)
			{
				result += synchronizePrefix(service1, service2, prefix + HashUtils.BYTE_TO_CHAR[i]);
			}

			return result;
		}
		else
		{
			List<Versioned<T>> items1 = service1.findByIdPrefix(prefix);
			List<Versioned<T>> items2 = service2.findByIdPrefix(prefix);

			return processItems(service1, service2, items1, items2);
		}
	}

	/**
	 * Synchronizes two object services
	 * 
	 * @param <T>
	 * 
	 * @param service1
	 *            First service
	 * @param service2
	 *            Second service
	 * @return Total number of transferred items
	 */
	public static <T> int synchronize(ObjectService<T> service1, ObjectService<T> service2)
	{
		// null checks
		if (null == service1)
		{
			throw new IllegalArgumentException("service1 is null");
		}
		if (null == service2)
		{
			throw new IllegalArgumentException("service2 is null");
		}

		return synchronizePrefix(service1, service2, "");
	}

	/* ============================ SYNC METHODS: HASH-BASED (v2) ============================ */

	private static <T> int synchronizeChildren(ObjectService<T> service1, MerkleTree tree1, ObjectService<T> service2,
			MerkleTree tree2, String prefix, ProgressCallback callback)
	{
		if (callback != null)
		{
			switch (prefix.length())
			{
				case 0:
				{
					callback.update(0, 256);
					break;
				}
				case 1:
				{
					int progress = HashUtils.CHAR_TO_BYTE[prefix.charAt(0)] * 16;
					callback.update(progress, 256);
					break;
				}
				case 2:
				{
					int progress = HashUtils.CHAR_TO_BYTE[prefix.charAt(0)] * 16
							+ HashUtils.CHAR_TO_BYTE[prefix.charAt(1)];
					callback.update(progress, 256);
					break;
				}
			}
		}

		if (prefix.length() < ObjectService.ID_PREFIX_SIZE)
		{
			int count1 = service1.count(prefix);
			int count2 = service2.count(prefix);

			if (count1 <= ObjectService.MAX_ITEMS_COUNT && count2 <= ObjectService.MAX_ITEMS_COUNT)
			{
				// there are not too many items, process it at once
				List<Versioned<T>> items1 = service1.findByIdPrefix(prefix);
				List<Versioned<T>> items2 = service2.findByIdPrefix(prefix);
				return processItems(service1, service2, items1, items2);
			}
			else
			{
				// ok, need for finer separation
				Map<String, String> hashes1 = tree1.getHashChildren(prefix);
				Map<String, String> hashes2 = tree2.getHashChildren(prefix);
				int result = 0;
				for (int i = 0; i < HashUtils.PATTERN_SIZE; i++)
				{
					String key = prefix + HashUtils.BYTE_TO_CHAR[i];
					String hash1 = hashes1.get(key);
					String hash2 = hashes2.get(key);
					if (!Utils.equals(hash1, hash2))
					{
						result += synchronizeChildren(service1, tree1, service2, tree2, key, callback);
					}
				}

				return result;
			}
		}
		else
		{
			List<Versioned<T>> items1 = service1.findByIdPrefix(prefix);
			List<Versioned<T>> items2 = service2.findByIdPrefix(prefix);

			return processItems(service1, service2, items1, items2);
		}
	}

	public static <T> int synchronize_v2(ObjectService<T> service1, ObjectService<T> service2, ProgressCallback callback)
	{
		// null checks
		if (null == service1)
		{
			throw new IllegalArgumentException("service1 is null");
		}
		if (null == service2)
		{
			throw new IllegalArgumentException("service2 is null");
		}

		if (callback != null)
		{
			callback.update(0, 256);
		}

		MerkleTree tree1 = service1.getHashTree();
		MerkleTree tree2 = service2.getHashTree();

		String hash1 = tree1.getHash("");
		String hash2 = tree2.getHash("");
		int count;
		if (!Utils.equals(hash1, hash2))
		{
			count = synchronizeChildren(service1, tree1, service2, tree2, "", callback);
		}
		else
		{
			count = 0;
		}

		if (callback != null)
		{
			callback.update(256, 256);
		}

		return count;
	}

	/**
	 * Synchronizes only items presented in both services
	 * 
	 * @param preferences1
	 * @param preferences2
	 * @return True if any data was transferred, false otherwise (i.e. if services already had been synchronized)
	 */
	public static boolean synchronizePreferences(PreferencesService preferences1, PreferencesService preferences2)
	{
		String hash1 = preferences1.getHash();
		String hash2 = preferences2.getHash();

		if (hash1 != null && hash1.equals(hash2))
		{
			return false;
		}

		List<PreferenceEntry<String>> entries1 = preferences1.getAll();
		List<PreferenceEntry<String>> entries2 = preferences2.getAll();
		List<PreferenceEntry<String>> newer1 = new ArrayList<PreferenceEntry<String>>();
		List<PreferenceEntry<String>> newer2 = new ArrayList<PreferenceEntry<String>>();

		for (PreferenceEntry<String> e1 : entries1)
		{
			boolean found = false;
			for (PreferenceEntry<String> e2 : entries2)
			{
				if (e1.getType() == e2.getType())
				{
					found = true;
					if (e1.getVersion() > e2.getVersion())
					{
						newer1.add(e1);
					}
					else if (e1.getVersion() < e2.getVersion())
					{
						newer2.add(e2);
					}
					break;
				}
			}

			if (!found)
			{
				newer1.add(e1);
			}
		}

		for (PreferenceEntry<String> e2 : entries2)
		{
			boolean found = false;
			for (PreferenceEntry<String> e1 : entries1)
			{
				if (e1.getType() == e2.getType())
				{
					found = true;
					break;
				}
			}

			if (!found)
			{
				newer2.add(e2);
			}
		}

		preferences1.update(newer2);
		preferences2.update(newer1);

		return !newer1.isEmpty() || !newer2.isEmpty();
	}
}
