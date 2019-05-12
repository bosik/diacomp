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

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * Versioned entry
 */
// FIXME: Move this class away from merklesync project
public class Versioned<T> extends AbstractVersioned<T> implements Serializable
{
	private static final long serialVersionUID = 6063993499772711799L;

	private String  id;
	@JsonProperty("stamp")
	@JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm:ss")
	private Date    timeStamp;
	private String  hash;
	private int     version;
	private boolean deleted;

	private T data;

	// ================================ METHODS ================================

	public Versioned()
	{
		this((T) null);
	}

	public Versioned(T data)
	{
		this.version = 1;
		this.data = data;
		this.deleted = false;
	}

	public Versioned(Versioned<? extends T> object)
	{
		Utils.nullCheck(object, "object");

		copyMetadata(object);
		setData(object.getData());
	}

	// ================================ GET / SET ================================

	@Override
	public String getId()
	{
		return id;
	}

	@Override
	public void setId(String id)
	{
		if (id == null)
		{
			throw new IllegalArgumentException("ID is null");
		}

		this.id = id.toLowerCase();
	}

	@Override
	public Date getTimeStamp()
	{
		return timeStamp;
	}

	@Override
	public void setTimeStamp(Date timeStamp)
	{
		this.timeStamp = timeStamp;
	}

	@Override
	public String getHash()
	{
		return hash;
	}

	@Override
	public void setHash(String hash)
	{
		this.hash = hash;
	}

	@Override
	public int getVersion()
	{
		return version;
	}

	@Override
	public void setVersion(int version)
	{
		this.version = version;
	}

	@Override
	public boolean isDeleted()
	{
		return deleted;
	}

	@Override
	public void setDeleted(boolean deleted)
	{
		this.deleted = deleted;
	}

	@Override
	public T getData()
	{
		return data;
	}

	@Override
	public void setData(T data)
	{
		this.data = data;
	}

	// ================================ OTHER ================================

	// TODO: Migrate to Java 8 and move it to AbstractVersioned
	public static <T> List<Versioned<T>> wrap(Iterable<T> items)
	{
		List<Versioned<T>> result = new ArrayList<>();

		for (T item : items)
		{
			result.add(new Versioned<>(item));
		}

		return result;
	}

	public <X> Versioned<X> castTo(Class<X> cls)
	{
		Versioned<X> result = new Versioned<>();

		result.copyMetadata(this);
		result.setData((X) getData());

		return result;
	}
}
