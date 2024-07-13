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
package org.bosik.diacomp.core.entities.business.foodbase;

import com.fasterxml.jackson.annotation.JsonProperty;

import org.bosik.diacomp.core.entities.business.Food;
import org.bosik.diacomp.core.entities.business.interfaces.NamedRelative;

import java.util.Locale;

/**
 * Food item for food base
 */
public class FoodItem extends Food implements NamedRelative
{
	private static final long serialVersionUID = 1789285539891342521L;

	public static final String FIELD_TAG   = "tag";
	public static final String FIELD_TABLE = "table";

	@JsonProperty(FIELD_TAG)
	private int tag;

	@JsonProperty(FIELD_TABLE)
	private boolean fromTable;

	public FoodItem()
	{

	}

	public FoodItem(String name, double relProts, double relFats, double relCarbs, double relValue, int tag, boolean fromTable)
	{
		super(name, relProts, relFats, relCarbs, relValue);
		setTag(tag);
		setFromTable(fromTable);
	}

	public FoodItem(FoodItem food)
	{
		super(food.getName(), food.getRelProts(), food.getRelFats(), food.getRelCarbs(), food.getRelValue());
		setTag(food.getTag());
		setFromTable(food.getFromTable());
	}

	// ================================ GET / SET ================================

	@Deprecated
	public int getTag()
	{
		return tag;
	}

	@Deprecated
	public void setTag(int tag)
	{
		this.tag = tag;
	}

	public boolean getFromTable()
	{
		return fromTable;
	}

	public void setFromTable(boolean fromTable)
	{
		this.fromTable = fromTable;
	}

	// ================================ MISC ================================

	@Override
	public String toString()
	{
		return String.format(Locale.US, "%s[%.1f|%.1f|%.1f|%.1f]:%s", getName(), getRelProts(), getRelFats(), getRelCarbs(), getRelValue(),
				getFromTable());
	}
}
