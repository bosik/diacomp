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
import java.util.Locale;

public abstract class AbstractVersioned<T> implements IVersioned<T>
{
	public static <T> void regenerateIds(Iterable<? extends IVersioned<T>> items)
	{
		for (IVersioned<T> item : items)
		{
			item.setId(HashUtils.generateGuid());
		}
	}

	public void copyMetadata(IVersioned<?> source)
	{
		copyMetadata(source, this);
	}

	public static void copyMetadata(IVersioned<?> source, IVersioned<?> destination)
	{
		destination.setId(source.getId());
		destination.setTimeStamp(source.getTimeStamp());
		destination.setHash(source.getHash());
		destination.setVersion(source.getVersion());
		destination.setDeleted(source.isDeleted());
	}

	public void modified()
	{
		setVersion(getVersion() + 1);
		setHash(HashUtils.generateGuid());
		setTimeStamp(new Date());
	}

	@Override
	public int hashCode()
	{
		final int prime = 31;
		int result = 1;
		result = (prime * result) + ((getId() == null) ? 0 : getId().hashCode());
		return result;
	}

	@Override
	@SuppressWarnings("unchecked")
	public boolean equals(Object obj)
	{
		if (this == obj)
			return true;
		if (!(obj instanceof IVersioned))
			return false;
		IVersioned<T> other = (IVersioned<T>) obj;
		if (getId() == null)
		{
			if (other.getId() != null)
				return false;
		}
		else if (!getId().equals(other.getId()))
			return false;
		return true;
	}

	@Override
	public String toString()
	{
		return String
				.format(Locale.US, "(ID=%s, Timestamp=%s, Hash=%s, Version=%d, Deleted=%s, Data=%s)", getId(), getTimeStamp(), getHash(),
						getVersion(), isDeleted(), getData());
	}

	public static <T> List<T> unwrap(Iterable<? extends IVersioned<T>> items)
	{
		List<T> result = new ArrayList<>();

		for (IVersioned<T> item : items)
		{
			result.add(item.getData());
		}

		return result;
	}
}
