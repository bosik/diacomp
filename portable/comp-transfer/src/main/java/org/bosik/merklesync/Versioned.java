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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Locale;

/**
 * Versioned entry
 */
public class Versioned<T> implements Serializable
{
	private static final long serialVersionUID = 6063993499772711799L;

	private String  id;
	private Date    timeStamp;
	private String  hash;
	private int     version;
	private boolean deleted;

	private T data;

	// ================================ UTILS ================================

	public static final Comparator<Versioned<?>> COMPARATOR_GUID = new Comparator<Versioned<?>>()
	{
		@Override
		public int compare(Versioned<?> lhs, Versioned<?> rhs)
		{
			return lhs.getId().compareTo(rhs.getId());
		}
	};

	// ================================ METHODS ================================

	public Versioned()
	{
		this((T) null);
	}

	public Versioned(T data)
	{
		this.version = 0;
		this.data = data;
		this.deleted = false;
		modified();
	}

	public Versioned(Versioned<? extends T> object)
	{
		Utils.nullCheck(object, "object");

		copyMetadata(object);
		setData(object.getData());
	}

	public void modified()
	{
		version++;
		hash = HashUtils.generateGuid();
		timeStamp = new Date();
	}

	// ================================ GET / SET ================================

	public String getId()
	{
		return id;
	}

	public void setId(String id)
	{
		if (id == null)
		{
			throw new IllegalArgumentException("ID is null");
		}

		this.id = id.toLowerCase();
	}

	public Date getTimeStamp()
	{
		return timeStamp;
	}

	public void setTimeStamp(Date timeStamp)
	{
		this.timeStamp = timeStamp;
	}

	public String getHash()
	{
		return hash;
	}

	public void setHash(String hash)
	{
		this.hash = hash;
	}

	public int getVersion()
	{
		return version;
	}

	public void setVersion(int version)
	{
		this.version = version;
	}

	public boolean isDeleted()
	{
		return deleted;
	}

	public void setDeleted(boolean deleted)
	{
		this.deleted = deleted;
	}

	public T getData()
	{
		return data;
	}

	public void setData(T data)
	{
		this.data = data;
	}

	// ================================ OTHER ================================

	@Override
	public int hashCode()
	{
		final int prime = 31;
		int result = 1;
		result = (prime * result) + ((id == null) ? 0 : id.hashCode());
		return result;
	}

	@Override
	@SuppressWarnings("unchecked")
	public boolean equals(Object obj)
	{
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Versioned<T> other = (Versioned<T>) obj;
		if (id == null)
		{
			if (other.id != null)
				return false;
		}
		else if (!id.equals(other.id))
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

	public static <T> List<Versioned<T>> wrap(Iterable<T> items)
	{
		List<Versioned<T>> result = new ArrayList<Versioned<T>>();

		for (T item : items)
		{
			result.add(new Versioned<T>(item));
		}

		return result;
	}

	public static <T> List<T> unwrap(Iterable<Versioned<T>> items)
	{
		List<T> result = new ArrayList<T>();

		for (Versioned<T> item : items)
		{
			result.add(item.getData());
		}

		return result;
	}

	public static <T> void regenerateIds(Iterable<Versioned<T>> items)
	{
		for (Versioned<T> item : items)
		{
			item.setId(HashUtils.generateGuid());
		}
	}

	public static void copyMetadata(Versioned<?> source, Versioned<?> destination)
	{
		destination.setId(source.getId());
		destination.setTimeStamp(source.getTimeStamp());
		destination.setHash(source.getHash());
		destination.setVersion(source.getVersion());
		destination.setDeleted(source.isDeleted());
	}

	public void copyMetadata(Versioned<?> source)
	{
		copyMetadata(source, this);
	}

	public <X> Versioned<X> castTo(Class<X> cls)
	{
		Versioned<X> result = new Versioned<X>();

		result.copyMetadata(this);
		result.setData((X) getData());

		return result;
	}
}
