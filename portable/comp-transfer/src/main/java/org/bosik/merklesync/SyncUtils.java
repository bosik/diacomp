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

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

public class SyncUtils
{
	/* ============================ SYNC METHODS: NAIVE ============================ */

	/**
	 * Synchronizes two data sources, fetching all items modified after specified time and comparing items' versions
	 *
	 * @param service1      First service
	 * @param service2      Second service
	 * @param since         Only items modified after this time stamp will be synced
	 * @param maxItemsWrite Max number of items to be saved per request
	 * @param <T>           Type of data source objects
	 * @return Total number of transferred items
	 */
	public static <T> int synchronizeModifiedAfter(DataSource<T> service1, DataSource<T> service2, Date since, int maxItemsWrite)
	{
		Utils.nullCheck(service1, "service1");
		Utils.nullCheck(service2, "service2");
		Utils.nullCheck(since, "since date");

		final List<Versioned<T>> items1 = service1.findChanged(since);
		final List<Versioned<T>> items2 = service2.findChanged(since);
		return Utils.processItems(service1, service2, items1, items2, maxItemsWrite);
	}

	/**
	 * Synchronizes two data sources, fetching all items modified after specified time and comparing items' versions
	 *
	 * @param service1 First service
	 * @param service2 Second service
	 * @param since    Only items modified after this time stamp will be synced
	 * @param <T>      Type of data source objects
	 * @return Total number of transferred items
	 */
	public static <T> int synchronizeModifiedAfter(DataSource<T> service1, DataSource<T> service2, Date since)
	{
		return synchronizeModifiedAfter(service1, service2, since, Integer.MAX_VALUE);
	}

	/**
	 * Synchronizes a single given entry between provided data sources:
	 * <ul>
	 * <li>If entry isn't found in any of sources, nothing happens; 0 is returned</li>
	 * <li>If entry is found in only one source, it's copied to another source; 1 is returned</li>
	 * <li>If entry is found in both sources, versions are compared:
	 * <ul>
	 * <li>If versions are different, an entry with higher version is copied to another source; 1 is returned</li>
	 * <li>If versions are equal, nothing happens; 0 is returned</li>
	 * </ul>
	 * </li>
	 * </ul>
	 *
	 * @param service1 First service
	 * @param service2 Second service
	 * @param id       Entry ID
	 * @param <T>      Type of data source objects
	 * @return Total number of transferred items (either 0 or 1)
	 */
	public static <T> int synchronizeSingle(DataSource<T> service1, DataSource<T> service2, String id)
	{
		Utils.nullCheck(service1, "service1");
		Utils.nullCheck(service2, "service2");
		Utils.nullCheck(id, "id");

		// requesting items
		final Versioned<T> item1 = service1.findById(id);
		final Versioned<T> item2 = service2.findById(id);

		if (item1 == null)
		{
			if (item2 == null)
			{
				return 0; // item was not found in any sources
			}
			else
			{
				service1.save(Collections.singletonList(item2));
				return 1;
			}
		}

		if (item2 == null)
		{
			service2.save(Collections.singletonList(item1));
			return 1;
		}

		if (item1.getVersion() < item2.getVersion())
		{
			service1.save(Collections.singletonList(item2));
			return 1;
		}

		if (item1.getVersion() > item2.getVersion())
		{
			service2.save(Collections.singletonList(item1));
			return 1;
		}

		return 0;
	}

	/**
	 * Transfers a given entry from one source to another; no version comparison happens
	 *
	 * @param sourceFrom Original source
	 * @param sourceTo   Destination source
	 * @param id         Entry ID
	 * @param <T>        Type of data source objects
	 * @return Total number of transferred items (either 0 or 1)
	 */
	public static <T> int transferSingle(DataSource<T> sourceFrom, DataSource<T> sourceTo, String id)
	{
		final Versioned<T> entry = sourceFrom.findById(id);
		if (entry != null)
		{
			sourceTo.save(Collections.singletonList(entry));
			return 1;
		}
		else
		{
			return 0;
		}
	}

	/* ============================ SYNC METHODS: HASH-BASED ============================ */

	/**
	 * Synchronizes two data sources based on tree hashes and version comparison
	 *
	 * @param service1      First service
	 * @param service2      Second service
	 * @param maxItemsWrite Max number of items to be saved to service per request
	 * @param <T>           Type of data source objects
	 * @return Total number of transferred items
	 * @deprecated This implementation checks only one hash per request. Use {@link org.bosik.merklesync.SyncUtils#synchronizeByHashChildren(DataSource, DataSource, int, int) synchronizeByHashChildren} instead.
	 */
	@Deprecated
	public static <T> int synchronizeByHash(DataSource<T> service1, DataSource<T> service2, int maxItemsWrite)
	{
		Utils.nullCheck(service1, "service1");
		Utils.nullCheck(service2, "service2");

		final MerkleTree hashTree1 = Utils.nullCheck(service1.getHashTree(), "hashTree1");
		final MerkleTree hashTree2 = Utils.nullCheck(service2.getHashTree(), "hashTree2");

		return synchronizeHashPrefix(service1, service2, hashTree1, hashTree2, maxItemsWrite, "");
	}

	/**
	 * Synchronizes two data sources based on tree hashes and version comparison
	 *
	 * @param service1 First service
	 * @param service2 Second service
	 * @param <T>      Type of data source objects
	 * @return Total number of transferred items
	 * @deprecated This implementation checks only one hash per request. Use {@link org.bosik.merklesync.SyncUtils#synchronizeByHashChildren(DataSource, DataSource, int, int) synchronizeByHashChildren} instead.
	 */
	@Deprecated
	public static <T> int synchronizeByHash(DataSource<T> service1, DataSource<T> service2)
	{
		return synchronizeByHash(service1, service2, Integer.MAX_VALUE);
	}

	private static <T> int synchronizeHashPrefix(
			DataSource<T> service1,
			DataSource<T> service2,
			MerkleTree hashTree1,
			MerkleTree hashTree2,
			int maxItemsWrite,
			String prefix)
	{

		final String hash1 = hashTree1.getHash(prefix);
		final String hash2 = hashTree2.getHash(prefix);

		if (Utils.equals(hash1, hash2))
		{
			return 0;
		}

		if (prefix.length() < DataSource.ID_PREFIX_SIZE)
		{
			int result = 0;

			for (int i = 0; i < 16; i++)
			{
				result += synchronizeHashPrefix(service1, service2, hashTree1, hashTree2, maxItemsWrite, prefix + HashUtils.byteToChar(i));
			}

			return result;
		}
		else
		{
			List<Versioned<T>> items1 = service1.findByIdPrefix(prefix);
			List<Versioned<T>> items2 = service2.findByIdPrefix(prefix);

			return Utils.processItems(service1, service2, items1, items2, maxItemsWrite);
		}
	}

	/**
	 * Synchronize two data sources
	 *
	 * @param source1       First source
	 * @param source2       Second source
	 * @param maxItemsRead  Max number of items to be read from a source per request
	 * @param maxItemsWrite Max number of items to be saved to a source per request
	 */
	public static <T> int synchronizeByHashChildren(DataSource<T> source1, DataSource<T> source2, int maxItemsRead, int maxItemsWrite)
	{
		final BufferedDataSource<T> bufferedSource1 = new BufferedDataSource<>(Utils.nullCheck(source1, "source1"), maxItemsWrite);
		final BufferedDataSource<T> bufferedSource2 = new BufferedDataSource<>(Utils.nullCheck(source2, "source2"), maxItemsWrite);

		final AtomicInteger count = new AtomicInteger(0);

		compare(source1, source2, source1.getHashTree(), source2.getHashTree(), item ->
				{
					bufferedSource2.save(item);
					count.incrementAndGet();
				},
				item ->
				{
					bufferedSource1.save(item);
					count.incrementAndGet();
				},
				maxItemsRead
		);

		bufferedSource1.flush();
		bufferedSource2.flush();

		return count.get();
	}

	private static <T> void compare(
			DataSource<T> source1,
			DataSource<T> source2,
			MerkleTree hashTree1,
			MerkleTree hashTree2,
			Consumer<Versioned<T>> handlerNewer1,
			Consumer<Versioned<T>> handlerNewer2,
			int maxItemsRead)
	{
		final String hash1 = hashTree1.getHash("");
		final String hash2 = hashTree2.getHash("");

		if (!Utils.equals(hash1, hash2))
		{
			compareChildren(source1, source2, source1.getHashTree(), source2.getHashTree(), maxItemsRead, handlerNewer1, handlerNewer2, "");
		}
	}

	private static <T> void compareChildren(
			DataSource<T> source1,
			DataSource<T> source2,
			MerkleTree hashTree1,
			MerkleTree hashTree2,
			int maxItemsRead,
			Consumer<Versioned<T>> handlerNewer1,
			Consumer<Versioned<T>> handlerNewer2,
			String prefix)
	{
		if ((prefix.length() < DataSource.ID_PREFIX_SIZE)
				&& (source1.count(prefix) > maxItemsRead || source2.count(prefix) > maxItemsRead))
		{
			// ok, finer separation required
			final Map<String, String> hashes1 = hashTree1.getHashChildren(prefix);
			final Map<String, String> hashes2 = hashTree2.getHashChildren(prefix);

			for (int i = 0; i < 16; i++)
			{
				final String key = prefix + HashUtils.byteToChar(i);
				final String hash1 = hashes1.get(key);
				final String hash2 = hashes2.get(key);

				if (!Utils.equals(hash1, hash2))
				{
					compareChildren(source1, source2, hashTree1, hashTree2, maxItemsRead, handlerNewer1, handlerNewer2, key);
				}
			}
		}
		else
		{
			// we can't granulate anymore / there are not too many items, so we can process them all at once
			final List<Versioned<T>> items1 = source1.findByIdPrefix(prefix);
			final List<Versioned<T>> items2 = source2.findByIdPrefix(prefix);

			Utils.getOverLists(items1, items2, handlerNewer1, handlerNewer2);
		}
	}
}
