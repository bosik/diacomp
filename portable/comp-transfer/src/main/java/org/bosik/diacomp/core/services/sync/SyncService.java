package org.bosik.diacomp.core.services.sync;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.utils.Utils;

public class SyncService
{
	/* ============================ HELPER CLASSES ============================ */

	public static interface Callback
	{
		public void update_max(int max);

		public void update_progress(int progress);
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
			throw new NullPointerException("items1 can't be null");
		}
		if (null == items2)
		{
			throw new NullPointerException("items2 can't be null");
		}
		if (null == newer1)
		{
			throw new NullPointerException("newer1 can't be null");
		}
		if (null == newer2)
		{
			throw new NullPointerException("newer2 can't be null");
		}
		if (null == only1)
		{
			throw new NullPointerException("only1 can't be null");
		}
		if (null == only2)
		{
			throw new NullPointerException("only2 can't be null");
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
			throw new NullPointerException("Service1 returned null list");
		}
		if (null == items2)
		{
			throw new NullPointerException("Service2 returned null list");
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
			throw new NullPointerException("service1 can't be null");
		}
		if (null == service2)
		{
			throw new NullPointerException("service2 can't be null");
		}
		if (null == since)
		{
			throw new NullPointerException("since date can't be null");
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
			throw new NullPointerException("service1 can't be null");
		}
		if (null == service2)
		{
			throw new NullPointerException("service2 can't be null");
		}
		if (null == id)
		{
			throw new NullPointerException("id can't be null");
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
			throw new NullPointerException("service1 can't be null");
		}
		if (null == service2)
		{
			throw new NullPointerException("service2 can't be null");
		}

		return synchronizePrefix(service1, service2, "");
	}

	/* ============================ SYNC METHODS: HASH-BASED (v2) ============================ */

	private static <T> int synchronizeChildren(ObjectService<T> service1, ObjectService<T> service2, String prefix)
	{
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
				Map<String, String> hashes1 = service1.getHashChildren(prefix);
				Map<String, String> hashes2 = service2.getHashChildren(prefix);
				int result = 0;
				for (int i = 0; i < HashUtils.PATTERN_SIZE; i++)
				{
					String key = prefix + HashUtils.BYTE_TO_CHAR[i];
					String hash1 = hashes1.get(key);
					String hash2 = hashes2.get(key);
					if (!Utils.equals(hash1, hash2))
					{
						result += synchronizeChildren(service1, service2, key);
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

	public static <T> int synchronize_v2(ObjectService<T> service1, ObjectService<T> service2)
	{
		// null checks
		if (null == service1)
		{
			throw new NullPointerException("service1 can't be null");
		}
		if (null == service2)
		{
			throw new NullPointerException("service2 can't be null");
		}

		String hash1 = service1.getHash("");
		String hash2 = service2.getHash("");
		if (!Utils.equals(hash1, hash2))
		{
			return synchronizeChildren(service1, service2, "");
		}
		else
		{
			return 0;
		}
	}
}
