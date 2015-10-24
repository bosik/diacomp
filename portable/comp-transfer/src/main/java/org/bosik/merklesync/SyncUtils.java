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
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

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
			List<Versioned<T>> newer1, List<Versioned<T>> newer2)
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

		// preparation

		Collections.sort(items1, Versioned.COMPARATOR_GUID);
		Collections.sort(items2, Versioned.COMPARATOR_GUID);
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
				newer1.add(p1);
				i++;
			}
			else if (c > 0)
			{
				newer2.add(p2);
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
				else if (!p1.getHash().equals(p2.getHash()))
				{
					// We have a conflict
					if (p1.getTimeStamp().after(p2.getTimeStamp()))
					{
						newer1.add(p1);
					}
					else
					{
						newer2.add(p2);
					}
				}
				i++;
				j++;
			}
		}

		// finish first list
		while (i < items1.size())
		{
			newer1.add(items1.get(i));
			i++;
		}

		// finish second list
		while (j < items2.size())
		{
			newer2.add(items2.get(j));
			j++;
		}
	}

	private static <T> int processItems(DataSource<T> service1, DataSource<T> service2, List<Versioned<T>> items1,
			List<Versioned<T>> items2)
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
		getOverLists(items1, items2, newer1, newer2);

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
	public static <T> int synchronize(DataSource<T> service1, DataSource<T> service2, Date since)
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
	public static <T> int synchronize(DataSource<T> service1, DataSource<T> service2, String id)
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

	private static <T> int synchronizePrefix(DataSource<T> service1, DataSource<T> service2, String prefix)
	{
		String hash1 = service1.getHashTree().getHash(prefix);
		String hash2 = service2.getHashTree().getHash(prefix);

		if (SyncUtils.equals(hash1, hash2))
		{
			return 0;
		}

		if (prefix.length() < DataSource.ID_PREFIX_SIZE)
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
	public static <T> int synchronize(DataSource<T> service1, DataSource<T> service2)
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

	private static <T> void synchronizeChildren(DataSource<T> service1, MerkleTree tree1, DataSource<T> service2,
			MerkleTree tree2, String prefix, List<Versioned<T>> newer1, List<Versioned<T>> newer2,
			ProgressCallback callback)
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

		if ((prefix.length() < DataSource.ID_PREFIX_SIZE) && (service1.count(prefix) > DataSource.MAX_ITEMS_COUNT
				|| service2.count(prefix) > DataSource.MAX_ITEMS_COUNT))
		{
			// ok, need for finer separation
			Map<String, String> hashes1 = tree1.getHashChildren(prefix);
			Map<String, String> hashes2 = tree2.getHashChildren(prefix);

			for (int i = 0; i < HashUtils.PATTERN_SIZE; i++)
			{
				String key = prefix + HashUtils.BYTE_TO_CHAR[i];
				String hash1 = hashes1.get(key);
				String hash2 = hashes2.get(key);
				if (!SyncUtils.equals(hash1, hash2))
				{
					synchronizeChildren(service1, tree1, service2, tree2, key, newer1, newer2, callback);
				}
			}
		}
		else
		{
			// there are not too many items, process it at once
			List<Versioned<T>> items1 = service1.findByIdPrefix(prefix);
			List<Versioned<T>> items2 = service2.findByIdPrefix(prefix);
			getOverLists(items1, items2, newer1, newer2);
		}
	}

	public static <T> int synchronize_v2(DataSource<T> service1, DataSource<T> service2, ProgressCallback callback)
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

		List<Versioned<T>> newer1 = new ArrayList<Versioned<T>>();
		List<Versioned<T>> newer2 = new ArrayList<Versioned<T>>();

		if (!SyncUtils.equals(hash1, hash2))
		{
			synchronizeChildren(service1, tree1, service2, tree2, "", newer1, newer2, callback);
		}

		blockSave(newer1, service2);
		blockSave(newer2, service1);

		if (callback != null)
		{
			callback.update(256, 256);
		}

		return newer1.size() + newer2.size();
	}

	private static <T> void blockSave(List<Versioned<T>> items, DataSource<T> service)
	{
		for (int fromIndex = 0; fromIndex < items.size(); fromIndex += DataSource.MAX_ITEMS_COUNT)
		{
			int toIndex = Math.min(fromIndex + DataSource.MAX_ITEMS_COUNT, items.size());
			service.save(items.subList(fromIndex, toIndex));
		}
	}

	/**
	 * Null-safe check.
	 * 
	 * @param a
	 * @param b
	 * @return True if both strings are null or equal
	 */
	private static boolean equals(String a, String b)
	{
		return a != null && a.equals(b) || a == b;
	}
}
