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

/**
 * Stores food's name, relative parameters (PFCV on 100g) and mass.
 */
public class FoodMassed extends Food
{
	private static final long	serialVersionUID	= -204761623726950977L;

	private double				mass;

	public FoodMassed()
	{
	}

	public FoodMassed(String name, double relProts, double relFats, double relCarbs, double relValue, double mass)
	{
		super(name, relProts, relFats, relCarbs, relValue);
		setMass(mass);
	}

	public FoodMassed(Food food, double mass)
	{
		super(food);
		setMass(mass);
	}

	// ================================ VALIDATORS ================================

	public static boolean checkMass(double value)
	{
		return checkNonNegativeValue(value);
	}

	// ================================ THROWERS ================================

	protected static void checkMassThrowable(double value)
	{
		checkAndThrow(checkMass(value), String.format("Mass can't be negative (%f)", value));
	}

	// ================================ GET / SET ================================

	public double getMass()
	{
		return mass;
	}

	public void setMass(double mass)
	{
		checkMassThrowable(mass);
		this.mass = mass;
	}

	public double getProts()
	{
		return (getRelProts() / 100.0) * mass;
	}

	public double getFats()
	{
		return (getRelFats() / 100.0) * mass;
	}

	public double getCarbs()
	{
		return (getRelCarbs() / 100.0) * mass;
	}

	public double getValue()
	{
		return (getRelValue() / 100.0) * mass;
	}

	@Override
	public String toString()
	{
		return String.format("%s [%.1f|%.1f|%.1f|%.1f]:%.1f", getName(), getRelProts(), getRelFats(), getRelCarbs(),
				getRelValue(), getMass());
	}
}
