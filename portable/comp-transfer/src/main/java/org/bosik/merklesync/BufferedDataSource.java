/*
 * MerkleSync - Data synchronization routine based on Merkle hash trees
 * Copyright (C) 2023 Nikita Bosik
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

/**
 * Proxy around regular DataSource; groups items provided to save() method to store them in chunks
 */
public class BufferedDataSource<T> implements DataSource<T>
{
	private final DataSource<T>      dataSource;
	private final int                bufferSize;
	private final List<Versioned<T>> buffer = new ArrayList<>();

	public BufferedDataSource(DataSource<T> dataSource, int bufferSize)
	{
		this.dataSource = dataSource;
		this.bufferSize = bufferSize;
	}

	@Override
	public void save(List<Versioned<T>> items)
	{
		if (buffer.size() + items.size() < bufferSize)
		{
			buffer.addAll(items);
		}
		else
		{
			// TODO: optimize
			for (Versioned<T> item : items)
			{
				buffer.add(item);
				if (buffer.size() >= bufferSize)
				{
					flush();
				}
			}
		}
	}

	public void save(Versioned<T> item)
	{
		buffer.add(item);

		if (buffer.size() >= bufferSize)
		{
			flush();
		}
	}

	public void flush()
	{
		if (!buffer.isEmpty())
		{
			dataSource.save(buffer);
			buffer.clear();
		}
	}

	// all the other methods are just proxies

	@Override
	public int count(String prefix)
	{
		return dataSource.count(prefix);
	}

	@Override
	public Versioned<T> findById(String id)
	{
		return dataSource.findById(id);
	}

	@Override
	public List<Versioned<T>> findByIdPrefix(String prefix)
	{
		return dataSource.findByIdPrefix(prefix);
	}

	@Override
	public List<Versioned<T>> findChanged(Date since)
	{
		return dataSource.findChanged(since);
	}

	@Override
	public String getHash(String prefix)
	{
		return dataSource.getHash(prefix);
	}

	@Override
	public Map<String, String> getHashChildren(String prefix)
	{
		return dataSource.getHashChildren(prefix);
	}
}
