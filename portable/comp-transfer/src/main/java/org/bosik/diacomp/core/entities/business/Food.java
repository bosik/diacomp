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
package org.bosik.diacomp.core.entities.business;

import java.io.Serializable;
import org.bosik.diacomp.core.entities.business.interfaces.Named;
import org.bosik.diacomp.core.entities.business.interfaces.Relative;

/**
 * Stores food's name and relative parameters (PFCV on 100g)
 */
public class Food implements Serializable, Named, Relative
{
	private static final long	serialVersionUID	= -659635365362405228L;

	private String				name;
	private double				relProts;
	private double				relFats;
	private double				relCarbs;
	private double				relValue;

	public Food()
	{

	}

	public Food(Food food)
	{
		this(food.getName(), food.getRelProts(), food.getRelFats(), food.getRelCarbs(), food.getRelValue());
	}

	public Food(String name, double relProts, double relFats, double relCarbs, double relValue)
	{
		setName(name);
		setRelProts(relProts);
		setRelFats(relFats);
		setRelCarbs(relCarbs);
		setRelValue(relValue);
	}

	// ================================ VALIDATORS ================================

	public static boolean checkName(String name)
	{
		return (name != null) && (!name.trim().equals(""));
	}

	public static boolean checkRelativeValue(double value)
	{
		return ((value >= 0) && (value <= 100));
	}

	public static boolean checkNonNegativeValue(double value)
	{
		return (value >= 0);
	}

	// ================================ THROWERS ================================

	protected static void checkAndThrow(boolean check, String errorMessage)
	{
		if (!check)
		{
			throw new IllegalArgumentException(errorMessage);
		}
	}

	protected static void checkNameThrowable(String name)
	{
		checkAndThrow(checkName(name), String.format("Name can't be null or empty (%s)", name));
	}

	protected static void checkRelThrowable(double value)
	{
		checkAndThrow(checkRelativeValue(value), String.format("Relative value (%f) is out of [0, 100] bounds", value));
	}

	protected static void checkNonNegativeThrowable(double value)
	{
		checkAndThrow(checkNonNegativeValue(value), String.format("Value can't be negative (%f)", value));
	}

	// ================================ GET / SET ================================

	@Override
	public String getName()
	{
		return name;
	}

	public void setName(String name)
	{
		//checkNameThrowable(name);
		this.name = name;
	}

	@Override
	public double getRelProts()
	{
		return relProts;
	}

	@Override
	public double getRelFats()
	{
		return relFats;
	}

	@Override
	public double getRelCarbs()
	{
		return relCarbs;
	}

	@Override
	public double getRelValue()
	{
		return relValue;
	}

	public void setRelProts(double relProts)
	{
		checkRelThrowable(relProts);
		this.relProts = relProts;
	}

	public void setRelFats(double relFats)
	{
		checkRelThrowable(relFats);
		this.relFats = relFats;
	}

	public void setRelCarbs(double relCarbs)
	{
		checkRelThrowable(relCarbs);
		this.relCarbs = relCarbs;
	}

	public void setRelValue(double relValue)
	{
		checkNonNegativeThrowable(relValue);
		this.relValue = relValue;
	}
}
