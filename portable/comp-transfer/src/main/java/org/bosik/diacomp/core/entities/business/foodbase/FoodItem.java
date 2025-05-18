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

	public static final String FIELD_TABLE                 = "table";
	public static final String FIELD_LAST_USED_IN_DIARY    = "luDiary";
	public static final String FIELD_LAST_USED_IN_DISHBASE = "luDishbase";

	@JsonProperty(FIELD_LAST_USED_IN_DIARY)
	private Long lastUsedInDiary;

	@JsonProperty(FIELD_LAST_USED_IN_DISHBASE)
	private Long lastUsedInDishBase;

	@JsonProperty(FIELD_TABLE)
	private boolean fromTable;

	public FoodItem()
	{
	}

	public FoodItem(FoodItem food)
	{
		super(food.getName(), food.getRelProts(), food.getRelFats(), food.getRelCarbs(), food.getRelValue());

		this.setFromTable(food.getFromTable());
		this.setLastUsedInDiary(food.getLastUsedInDiary());
		this.setLastUsedInDishBase(food.getLastUsedInDishBase());
	}

	// ================================ GET / SET ================================

	public boolean getFromTable()
	{
		return fromTable;
	}

	public void setFromTable(boolean fromTable)
	{
		this.fromTable = fromTable;
	}

	public Long getLastUsedInDiary()
	{
		return lastUsedInDiary;
	}

	public void setLastUsedInDiary(Long lastUsedInDiary)
	{
		this.lastUsedInDiary = lastUsedInDiary;
	}

	public Long getLastUsedInDishBase()
	{
		return lastUsedInDishBase;
	}

	public void setLastUsedInDishBase(Long lastUsedInDishBase)
	{
		this.lastUsedInDishBase = lastUsedInDishBase;
	}

	// ================================ MISC ================================

	@Override
	public String toString()
	{
		return String.format(Locale.US, "%s[%.1f|%.1f|%.1f|%.1f]:%s", getName(), getRelProts(), getRelFats(), getRelCarbs(), getRelValue(),
				getFromTable());
	}
}
