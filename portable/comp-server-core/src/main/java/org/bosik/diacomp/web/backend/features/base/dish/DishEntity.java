/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.web.backend.features.base.dish;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Date;

@Entity
@Table(name = "dish")
public class DishEntity
{
	@Id
	@Column(name = "id", columnDefinition = "CHAR(32)")
	private String id;

	@Column(name = "user_id")
	private int userId;

	@Column(name = "time_stamp")
	private Date timeStamp;

	@Column(name = "hash", columnDefinition = "CHAR(32)")
	private String hash;

	@Column(name = "version")
	private int version;

	@Column(name = "deleted")
	private boolean deleted;

	@Column(name = "content", columnDefinition = "TEXT")
	private String content;

	@Column(name = "name_cache")
	private String nameCache;

	public String getId()
	{
		return id;
	}

	public void setId(String id)
	{
		this.id = id;
	}

	public int getUserId()
	{
		return userId;
	}

	public void setUserId(int userId)
	{
		this.userId = userId;
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

	public String getContent()
	{
		return content;
	}

	public void setContent(String content)
	{
		this.content = content;
	}

	public String getNameCache()
	{
		return nameCache;
	}

	public void setNameCache(String nameCache)
	{
		this.nameCache = nameCache;
	}
}
