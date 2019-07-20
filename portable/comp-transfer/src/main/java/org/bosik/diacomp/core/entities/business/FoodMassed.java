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

import com.fasterxml.jackson.annotation.JsonIgnore;
import org.bosik.diacomp.core.entities.business.interfaces.Massed;
import org.bosik.diacomp.core.entities.business.interfaces.Named;
import org.bosik.diacomp.core.entities.business.interfaces.Relative;

import java.util.Locale;

/**
 * Stores food's name, relative parameters (PFCV on 100g) and mass.
 */
public class FoodMassed extends Food implements Massed
{
	private static final long serialVersionUID = -204761623726950977L;

	private double mass;

	public FoodMassed()
	{
	}

	public FoodMassed(String name, double relProts, double relFats, double relCarbs, double relValue, double mass)
	{
		super(name, relProts, relFats, relCarbs, relValue);
		setMass(mass);
	}

	public <T extends Named & Relative> FoodMassed(T src, double mass)
	{
		super(src);
		setMass(mass);
	}

	public <T extends Named & Relative & Massed> FoodMassed(T src)
	{
		super(src);
		setMass(src.getMass());
	}

	// ================================ VALIDATORS ================================

	public static boolean checkMass(double value)
	{
		return checkNonNegativeValue(value);
	}

	// ================================ THROWERS ================================

	protected static void checkMassThrowable(double value)
	{
		if (!checkMass(value))
		{
			throw new IllegalArgumentException(String.format("Mass can't be negative (%f)", value));
		}
	}

	// ================================ GET / SET ================================

	@Override
	public double getMass()
	{
		return mass;
	}

	public void setMass(double mass)
	{
		checkMassThrowable(mass);
		this.mass = mass;
	}

	@JsonIgnore
	public double getProts()
	{
		return (getRelProts() / 100.0) * mass;
	}

	@JsonIgnore
	public double getFats()
	{
		return (getRelFats() / 100.0) * mass;
	}

	@JsonIgnore
	public double getCarbs()
	{
		return (getRelCarbs() / 100.0) * mass;
	}

	@JsonIgnore
	public double getValue()
	{
		return (getRelValue() / 100.0) * mass;
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
			return true;
		if (!(o instanceof FoodMassed))
			return false;

		FoodMassed that = (FoodMassed) o;

		return Double.compare(that.mass, mass) == 0;
	}

	@Override
	public int hashCode()
	{
		long temp = Double.doubleToLongBits(mass);
		return (int) (temp ^ (temp >>> 32));
	}

	@Override
	public String toString()
	{
		return String.format(Locale.US, "%s [%.1f|%.1f|%.1f|%.1f]:%.1f", getName(), getRelProts(), getRelFats(), getRelCarbs(),
				getRelValue(), getMass());
	}
}
