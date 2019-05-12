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
import java.util.Objects;

public class BaseVersioned<T> extends AbstractVersioned<T>
{
	private String  id;
	private Date    timeStamp;
	private String  hash;
	private int     version;
	private boolean deleted;
	private T       data;

	public BaseVersioned()
	{
		this((T) null);
	}

	public BaseVersioned(T data)
	{
		this.version = 1;
		this.data = data;
		this.deleted = false;
	}

	public BaseVersioned(IVersioned<? extends T> object)
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
		this.id = id;
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
}
