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
package org.bosik.diacomp.core.entities.business.diary.records;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class MealRecord extends DiaryRecord
{
	private static final long serialVersionUID = -4920269773372985893L;

	public static final String TYPE = "meal";

	@JsonProperty("content")
	private final List<FoodMassed> items = new ArrayList<>();

	@JsonProperty("short")
	private boolean shortMeal;

	public MealRecord(Date time, boolean shortMeal)
	{
		setTime(time);
		setShortMeal(shortMeal);
	}

	public MealRecord()
	{
	}

	// ================================ GET / SET ================================

	@Override
	public String getType()
	{
		return TYPE;
	}

	public boolean getShortMeal()
	{
		return shortMeal;
	}

	public void setShortMeal(boolean value)
	{
		shortMeal = value;
	}

	// работа с характеристиками
	// TODO: написать тесты

	@JsonIgnore
	public double getProts()
	{
		double res = 0.0;
		for (FoodMassed item : items)
		{
			res += item.getProts();
		}

		return res;
	}

	@JsonIgnore
	public double getFats()
	{
		double res = 0.0;
		for (FoodMassed item : items)
		{
			res += item.getFats();
		}

		return res;
	}

	@JsonIgnore
	public double getCarbs()
	{
		double res = 0.0;
		for (FoodMassed item : items)
		{
			res += item.getCarbs();
		}

		return res;
	}

	@JsonIgnore
	public double getValue()
	{
		double res = 0.0;
		for (FoodMassed item : items)
		{
			res += item.getValue();
		}

		return res;
	}

	@JsonIgnore
	public double getMass()
	{
		double res = 0.0;
		for (FoodMassed item : items)
		{
			res += item.getMass();
		}

		return res;
	}

	// ============================== РАБОТА СО СПИСКОМ ==============================

	// работа со списком

	public int add(FoodMassed item)
	{
		if (item == null)
		{
			throw new IllegalArgumentException("FoodItem item is null");
		}

		items.add(item);
		return items.size() - 1;
	}

	public void clear()
	{
		items.clear();
	}

	public FoodMassed get(int index)
	{
		return items.get(index);
	}

	public int count()
	{
		return items.size();
	}

	public void remove(int index)
	{
		items.remove(index);
	}

	public List<FoodMassed> getItems()
	{
		return items;
	}

	@Override
	public String toString()
	{
		StringBuilder s = new StringBuilder();
		s.append("Time: ").append(getTime()).append('\n');
		s.append("Short postprand: ").append(getShortMeal()).append('\n');

		for (FoodMassed item : items)
		{
			s.append(item).append('\n');
		}

		return s.toString();
	}
}
