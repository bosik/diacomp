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
package org.bosik.diacomp.core.entities.business.dishbase;

import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.business.interfaces.NamedRelative;
import org.bosik.diacomp.core.entities.business.interfaces.Tagged;
import org.bosik.diacomp.core.utils.Utils;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Note: no check (rel* < 100) is presented
 */

public class DishItem implements NamedRelative, Tagged, Serializable
{
	private static final long serialVersionUID = 1L;

	private String name;
	private int    tag;
	private Double mass;

	private final List<FoodMassed> content = new ArrayList<FoodMassed>();

	// ================================ GET / SET ================================

	private double getRealMass()
	{
		if (mass != null)
		{
			return mass;
		}

		double result = 0.0;
		for (FoodMassed item : content)
		{
			result += item.getMass();
		}
		return result;
	}

	public Double getMass()
	{
		return mass;
	}

	public void setMass(Double mass)
	{
		if (mass == null)
		{
			this.mass = mass;
		}
		else
		{
			setMass((double) mass);
		}
	}

	public void setMass(double mass)
	{
		if (mass > Utils.EPS)
		{
			this.mass = mass;
		}
		else
		{
			throw new IllegalArgumentException(String.format("Incorrect mass: %f", mass));
		}
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
	public int getTag()
	{
		return tag;
	}

	@Override
	public void setTag(int tag)
	{
		this.tag = tag;
	}

	private double getRel(double total)
	{
		double realMass = getRealMass();
		if (realMass > Utils.EPS)
		{
			return total / realMass * 100.0;
		}
		else
		{
			return 0.0;
		}
	}

	@Override
	public double getRelProts()
	{
		double total = 0.0;
		for (FoodMassed item : content)
		{
			total += item.getProts();
		}

		return getRel(total);
	}

	@Override
	public double getRelFats()
	{
		double result = 0.0;
		for (FoodMassed item : content)
		{
			result += item.getFats();
		}

		return getRel(result);
	}

	@Override
	public double getRelCarbs()
	{
		double total = 0.0;
		for (FoodMassed item : content)
		{
			total += item.getCarbs();
		}

		return getRel(total);
	}

	@Override
	public double getRelValue()
	{
		double total = 0.0;
		for (FoodMassed item : content)
		{
			total += item.getValue();
		}

		return getRel(total);
	}

	// =================================== LIST METHODS ===================================

	public void add(FoodMassed item)
	{
		if (item != null)
		{
			content.add(item);
		}
		else
		{
			throw new IllegalArgumentException("Dish item is null");
		}
	}

	public void clear()
	{
		content.clear();
	}

	public void remove(int index)
	{
		content.remove(index);
	}

	public int count()
	{
		return content.size();
	}

	public FoodMassed get(int index)
	{
		return content.get(index);
	}

	// =================================== OTHER ===================================

	public FoodItem convertToFood()
	{
		FoodItem food = new FoodItem();

		food.setName(getName());
		food.setRelProts(getRelProts());
		food.setRelFats(getRelFats());
		food.setRelCarbs(getRelCarbs());
		food.setRelValue(getRelValue());
		food.setFromTable(false);
		food.setTag(getTag());

		return food;
	}
}
