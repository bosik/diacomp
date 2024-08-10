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
import java.util.List;
import java.util.function.Consumer;

public class Utils
{
	/**
	 * Null-safe check.
	 *
	 * @param a
	 * @param b
	 * @return True if both strings are null or equal
	 */
	public static boolean equals(String a, String b)
	{
		return a != null && a.equals(b) || a == b;
	}

	/**
	 * Verifies if the object is <code>null</code>
	 *
	 * @param object Object to check
	 * @param name   Object name (to build exception message in case it's null)
	 * @return Object itself, never <code>null</code>
	 * @throws IllegalArgumentException If the value is null
	 */
	public static <T> T nullCheck(T object, String name)
	{
		if (object == null)
		{
			throw new IllegalArgumentException(name + " is null");
		}
		else
		{
			return object;
		}
	}

	/**
	 * Calculates lists for synchronization
	 *
	 * @param items1 First list
	 * @param items2 Second list
	 * @param newer1 Callback for items that have greater version in the first list
	 * @param newer2 Callback for items that have greater version in the second list
	 */
	public static <T> void getOverLists(List<Versioned<T>> items1, List<Versioned<T>> items2, Consumer<Versioned<T>> newer1,
			Consumer<Versioned<T>> newer2)
	{
		nullCheck(items1, "items1");
		nullCheck(items2, "items2");
		nullCheck(newer1, "newer1");
		nullCheck(newer2, "newer2");

		// preparation
		items1.sort(Versioned.COMPARATOR_GUID);
		items2.sort(Versioned.COMPARATOR_GUID);
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
				newer1.accept(p1);
				i++;
			}
			else if (c > 0)
			{
				newer2.accept(p2);
				j++;
			}
			else
			{
				if (p1.getVersion() > p2.getVersion())
				{
					newer1.accept(p1);
				}
				else if (p1.getVersion() < p2.getVersion())
				{
					newer2.accept(p2);
				}
				else if (!p1.getHash().equals(p2.getHash()))
				{
					// We have a conflict
					if (p1.getTimeStamp().after(p2.getTimeStamp()))
					{
						newer1.accept(p1);
					}
					else
					{
						newer2.accept(p2);
					}
				}
				i++;
				j++;
			}
		}

		// finish first list
		while (i < items1.size())
		{
			newer1.accept(items1.get(i));
			i++;
		}

		// finish second list
		while (j < items2.size())
		{
			newer2.accept(items2.get(j));
			j++;
		}
	}

	/**
	 * Calculates lists for synchronization
	 *
	 * @param items1 First list
	 * @param items2 Second list
	 * @param newer1 Items which has greater version in the first list
	 * @param newer2 Items which has greater version in the second list
	 */
	public static <T> void getOverLists(List<Versioned<T>> items1, List<Versioned<T>> items2, List<Versioned<T>> newer1,
			List<Versioned<T>> newer2)
	{
		nullCheck(items1, "items1");
		nullCheck(items2, "items2");
		nullCheck(newer1, "newer1");
		nullCheck(newer2, "newer2");

		getOverLists(items1, items2, newer1::add, newer2::add);
	}

	/**
	 * Saves items to the service, transferring at most <code>blockSize</code> items per request
	 *
	 * @param items     Items to save
	 * @param service   Service to use
	 * @param blockSize Max number of items to be transferred per request
	 */
	public static <T> void blockSave(List<Versioned<T>> items, DataSource<T> service, int blockSize)
	{
		if (blockSize < Integer.MAX_VALUE)
		{
			for (int fromIndex = 0; fromIndex < items.size(); fromIndex += blockSize)
			{
				int toIndex = Math.min(fromIndex + blockSize, items.size());
				service.save(items.subList(fromIndex, toIndex));
			}
		}
		else
		{
			service.save(items);
		}
	}

	public static <T> int processItems(DataSource<T> service1, DataSource<T> service2, List<Versioned<T>> items1,
			List<Versioned<T>> items2, int maxItemsWrite)
	{
		nullCheck(service1, "service1");
		nullCheck(service2, "service2");
		nullCheck(items1, "items1");
		nullCheck(items2, "items2");

		// calculating transferring lists
		List<Versioned<T>> newer1 = new ArrayList<>();
		List<Versioned<T>> newer2 = new ArrayList<>();
		getOverLists(items1, items2, newer1, newer2);

		// transfer
		blockSave(newer2, service1, maxItemsWrite);
		blockSave(newer1, service2, maxItemsWrite);

		// Result is number of transferred records
		return newer1.size() + newer2.size();
	}
}
