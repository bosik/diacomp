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
package org.bosik.diacomp.web.backend.features.base.food.user;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;
import org.bosik.diacomp.web.backend.features.base.food.combo.FoodEntity;

import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.util.Date;

@Entity
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "food_user")
public class FoodUserEntity implements FoodEntity
{
	public static final int MAX_SIZE_NAME = 100;

	@EmbeddedId
	private FoodUserEntityPK key;

	@Column(name = "name", length = MAX_SIZE_NAME)
	private String name;

	@Column(name = "prots", columnDefinition = "DECIMAL")
	private double prots;

	@Column(name = "fats", columnDefinition = "DECIMAL")
	private double fats;

	@Column(name = "carbs", columnDefinition = "DECIMAL")
	private double carbs;

	@Column(name = "value", columnDefinition = "DECIMAL")
	private double value;

	@Column(name = "from_table")
	private boolean fromTable;

	@Column(name = "deleted")
	private boolean deleted;

	@Column(name = "last_modified")
	private Date lastModified;

	@Column(name = "hash", columnDefinition = "CHAR(32)")
	private String hash;

	@Column(name = "version")
	private int version;

	public FoodUserEntityPK getKey()
	{
		return key;
	}

	public void setKey(FoodUserEntityPK key)
	{
		this.key = key;
	}

	@Override
	public String getId()
	{
		return key.getId();
	}

	@Override
	public String getName()
	{
		return name;
	}

	public void setName(String name)
	{
		this.name = name;
	}

	@Override
	public double getProts()
	{
		return prots;
	}

	public void setProts(double prots)
	{
		this.prots = prots;
	}

	@Override
	public double getFats()
	{
		return fats;
	}

	public void setFats(double fats)
	{
		this.fats = fats;
	}

	@Override
	public double getCarbs()
	{
		return carbs;
	}

	public void setCarbs(double carbs)
	{
		this.carbs = carbs;
	}

	@Override
	public double getValue()
	{
		return value;
	}

	public void setValue(double value)
	{
		this.value = value;
	}

	@Override
	public boolean isFromTable()
	{
		return fromTable;
	}

	public void setFromTable(boolean fromTable)
	{
		this.fromTable = fromTable;
	}

	@Override
	public boolean isDeleted()
	{
		return deleted;
	}

	public void setDeleted(boolean deleted)
	{
		this.deleted = deleted;
	}

	@Override
	public Date getLastModified()
	{
		return lastModified;
	}

	public void setLastModified(Date lastModified)
	{
		this.lastModified = lastModified;
	}

	@Override
	public String getHash()
	{
		return hash;
	}

	public void setHash(String hash)
	{
		this.hash = hash;
	}

	@Override
	public int getVersion()
	{
		return version;
	}

	public void setVersion(int version)
	{
		this.version = version;
	}
}
