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
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

public class FakeObjectService implements DataSource<String>
{
	private final List<Versioned<String>> data = new ArrayList<>();
	private MerkleTree merkleTree;

	@Override
	public int count(String prefix)
	{
		int count = 0;

		for (Versioned<String> item : data)
		{
			if (item.getId().startsWith(prefix))
			{
				count++;
			}
		}

		return count;
	}

	@Override
	public Versioned<String> findById(String id)
	{
		for (Versioned<String> item : data)
		{
			if (item.getId().equals(id))
			{
				return new Versioned<>(item);
			}
		}
		return null;
	}

	@Override
	public List<Versioned<String>> findByIdPrefix(String prefix)
	{
		List<Versioned<String>> result = new ArrayList<>();

		for (Versioned<String> item : data)
		{
			if (item.getId().startsWith(prefix))
			{
				result.add(new Versioned<>(item));
			}
		}

		return result;
	}

	@Override
	public List<Versioned<String>> findChanged(Date since)
	{
		List<Versioned<String>> result = new ArrayList<>();

		for (Versioned<String> item : data)
		{
			if (item.getTimeStamp().after(since))
			{
				result.add(new Versioned<>(item));
			}
		}

		return result;
	}

	@Override
	public void save(List<Versioned<String>> items)
	{
		for (Versioned<String> item : items)
		{
			Versioned<String> temp = findById(item.getId());

			if (temp == null)
			{
				data.add(new Versioned<>(item));
			}
			else
			{
				for (Versioned<String> x : data)
				{
					if (x.getId().equals(item.getId()))
					{
						x.setTimeStamp(item.getTimeStamp());
						x.setHash(item.getHash());
						x.setVersion(item.getVersion());
						x.setDeleted(item.isDeleted());
						x.setData(item.getData());
						break;
					}
				}

			}
		}

		merkleTree = null;
	}

	private synchronized MerkleTree getHashTree()
	{
		if (merkleTree == null)
		{
			SortedMap<String, String> hashes = new TreeMap<>();
			for (Versioned<String> item : data)
			{
				hashes.put(item.getId(), item.getHash());
			}

			merkleTree = HashUtils.buildMerkleTree(hashes);
		}

		return merkleTree;
	}

	@Override
	public String getHash(String prefix)
	{
		return getHashTree().getHash(prefix);
	}

	@Override
	public Map<String, String> getHashChildren(String prefix)
	{
		return getHashTree().getHashChildren(prefix);
	}
}
