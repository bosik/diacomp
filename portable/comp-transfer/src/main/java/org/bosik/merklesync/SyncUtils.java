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
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

public class SyncUtils
{
	/* ============================ HELPER CLASSES ============================ */

	public interface ProgressCallback
	{
		/**
		 * Called on progress change. The percentage value may be calculated as
		 * <code>progress * 100 / max</code>
		 *
		 * @param progress Current progress value
		 * @param max      Out-of value
		 */
		void onProgress(int progress, int max);
	}

	public interface Synchronizer
	{
		/**
		 * Perform synchronizing
		 *
		 * @return Total number of transferred items
		 */
		int synchronize();
	}

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
	 * General-purpose data source synchronizer based on tree hashes and version comparison.
	 *
	 * @param <T> Type of data source objects
	 * @deprecated This implementation checks only one hash per request and doesn't support progress callbacking. Use
	 * {@link org.bosik.merklesync.SyncUtils.Synchronizer2 Synchronizer2} instead.
	 */
	@Deprecated
	public static class Synchronizer1<T> implements Synchronizer
	{
		private final DataSource<T> service1;
		private final DataSource<T> service2;
		private       MerkleTree    tree1;
		private       MerkleTree    tree2;
		private final int           maxItemsWrite;

		/**
		 * Constructor
		 *
		 * @param service1      First service
		 * @param service2      Second service
		 * @param maxItemsWrite Max number of items to be saved to service per request
		 */
		public Synchronizer1(DataSource<T> service1, DataSource<T> service2, int maxItemsWrite)
		{
			this.service1 = Utils.nullCheck(service1, "service1");
			this.service2 = Utils.nullCheck(service2, "service2");
			this.maxItemsWrite = maxItemsWrite;
		}

		/**
		 * Constructor
		 *
		 * @param service1 First service
		 * @param service2 Second service
		 */
		public Synchronizer1(DataSource<T> service1, DataSource<T> service2)
		{
			this(service1, service2, Integer.MAX_VALUE);
		}

		@Override
		public int synchronize()
		{
			tree1 = Utils.nullCheck(service1.getHashTree(), "tree1");
			tree2 = Utils.nullCheck(service2.getHashTree(), "tree2");
			return synchronizePrefix("");
		}

		private int synchronizePrefix(String prefix)
		{
			String hash1 = tree1.getHash(prefix);
			String hash2 = tree2.getHash(prefix);

			if (Utils.equals(hash1, hash2))
			{
				return 0;
			}

			if (prefix.length() < DataSource.ID_PREFIX_SIZE)
			{
				int result = 0;

				for (char c : HashUtils.BYTE_TO_CHAR)
				{
					result += synchronizePrefix(prefix + c);
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
	}

	/**
	 * General-purpose data source synchronizer based on tree hashes and version comparison.
	 *
	 * @param <T> Type of data source objects
	 */
	public static class Synchronizer2<T> implements Synchronizer
	{
		private final DataSource<T>      service1;
		private final DataSource<T>      service2;
		private       MerkleTree         tree1;
		private       MerkleTree         tree2;
		private       List<Versioned<T>> newer1;
		private       List<Versioned<T>> newer2;
		private final int                maxItemsRead;
		private final int                maxItemsWrite;
		private final ProgressCallback   callback;

		/**
		 * Constructor
		 *
		 * @param service1      First service
		 * @param service2      Second service
		 * @param maxItemsRead  Max number of items to be read from service per request
		 * @param maxItemsWrite Max number of items to be saved to service per request
		 * @param callback      Callback to inform about sync progress (optional, may be <code>null<code>)
		 */
		public Synchronizer2(DataSource<T> service1, DataSource<T> service2, int maxItemsRead, int maxItemsWrite,
				ProgressCallback callback)
		{
			this.service1 = Utils.nullCheck(service1, "service1");
			this.service2 = Utils.nullCheck(service2, "service2");
			this.maxItemsRead = maxItemsRead;
			this.maxItemsWrite = maxItemsWrite;
			this.callback = callback;
		}

		/**
		 * Constructor
		 *
		 * @param service1      First service
		 * @param service2      Second service
		 * @param maxItemsRead  Max number of items to be read from service per request
		 * @param maxItemsWrite Max number of items to be saved to service per request
		 */
		public Synchronizer2(DataSource<T> service1, DataSource<T> service2, int maxItemsRead, int maxItemsWrite)
		{
			this(service1, service2, maxItemsRead, maxItemsWrite, null);
		}

		/**
		 * Constructor
		 *
		 * @param service1 First service
		 * @param service2 Second service
		 * @param callback Callback to inform about sync progress (optional, may be <code>null<code>)
		 */
		public Synchronizer2(DataSource<T> service1, DataSource<T> service2, ProgressCallback callback)
		{
			this(service1, service2, Integer.MAX_VALUE, Integer.MAX_VALUE, callback);
		}

		/**
		 * Constructor
		 *
		 * @param service1 First service
		 * @param service2 Second service
		 */
		public Synchronizer2(DataSource<T> service1, DataSource<T> service2)
		{
			this(service1, service2, Integer.MAX_VALUE, Integer.MAX_VALUE, null);
		}

		@Override
		public int synchronize()
		{
			if (callback != null)
			{
				callback.onProgress(0, 256);
			}

			newer1 = new ArrayList<>();
			newer2 = new ArrayList<>();
			tree1 = service1.getHashTree();
			tree2 = service2.getHashTree();
			String hash1 = tree1.getHash("");
			String hash2 = tree2.getHash("");

			if (!Utils.equals(hash1, hash2))
			{
				synchronizeChildren("");
				Utils.blockSave(newer1, service2, maxItemsWrite);
				Utils.blockSave(newer2, service1, maxItemsWrite);
			}

			if (callback != null)
			{
				callback.onProgress(256, 256);
			}

			return newer1.size() + newer2.size();
		}

		private void synchronizeChildren(String prefix)
		{
			if (callback != null)
			{
				switch (prefix.length())
				{
					case 0:
					{
						callback.onProgress(0, 256);
						break;
					}
					case 1:
					{
						int progress = HashUtils.CHAR_TO_BYTE[prefix.charAt(0)] * 16;
						callback.onProgress(progress, 256);
						break;
					}
					case 2:
					{
						int progress = HashUtils.CHAR_TO_BYTE[prefix.charAt(0)] * 16
								+ HashUtils.CHAR_TO_BYTE[prefix.charAt(1)];
						callback.onProgress(progress, 256);
						break;
					}
				}
			}

			if ((prefix.length() < DataSource.ID_PREFIX_SIZE)
					&& (service1.count(prefix) > maxItemsRead || service2.count(prefix) > maxItemsRead))
			{
				// ok, finer separation required
				Map<String, String> hashes1 = tree1.getHashChildren(prefix);
				Map<String, String> hashes2 = tree2.getHashChildren(prefix);

				for (char c : HashUtils.BYTE_TO_CHAR)
				{
					String key = prefix + c;
					String hash1 = hashes1.get(key);
					String hash2 = hashes2.get(key);
					if (!Utils.equals(hash1, hash2))
					{
						synchronizeChildren(key);
					}
				}
			}
			else
			{
				// we can't granulate anymore / there are not too many items, so we can process it at once
				List<Versioned<T>> items1 = service1.findByIdPrefix(prefix);
				List<Versioned<T>> items2 = service2.findByIdPrefix(prefix);
				Utils.getOverLists(items1, items2, newer1, newer2);

				if (newer1.size() > maxItemsWrite * 4)
				{
					Utils.blockSave(newer1, service2, maxItemsWrite);
					newer1.clear();
				}
				if (newer2.size() > maxItemsWrite * 4)
				{
					Utils.blockSave(newer2, service1, maxItemsWrite);
					newer2.clear();
				}
			}
		}
	}
}
