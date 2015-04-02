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
package org.bosik.diacomp.core.services.analyze.entities;

import java.util.Date;

public class PrimeRec
{
	private Date	date;
	private int		bloodInTime;
	private double	bloodInValue;
	private int		insTime;
	private double	insValue;
	private int		foodTime;
	private double	prots;
	private double	fats;
	private double	carbs;
	private int		bloodOutTime;
	private double	bloodOutValue;

	public Date getDate()
	{
		return date;
	}

	public void setDate(Date date)
	{
		this.date = date;
	}

	public int getBloodInTime()
	{
		return bloodInTime;
	}

	public void setBloodInTime(int bloodInTime)
	{
		this.bloodInTime = bloodInTime;
	}

	public double getBloodInValue()
	{
		return bloodInValue;
	}

	public void setBloodInValue(double bloodInValue)
	{
		this.bloodInValue = bloodInValue;
	}

	public int getInsTime()
	{
		return insTime;
	}

	public void setInsTime(int insTime)
	{
		this.insTime = insTime;
	}

	public double getInsValue()
	{
		return insValue;
	}

	public void setInsValue(double insValue)
	{
		this.insValue = insValue;
	}

	public int getFoodTime()
	{
		return foodTime;
	}

	public void setFoodTime(int foodTime)
	{
		this.foodTime = foodTime;
	}

	public double getProts()
	{
		return prots;
	}

	public void setProts(double prots)
	{
		this.prots = prots;
	}

	public double getFats()
	{
		return fats;
	}

	public void setFats(double fats)
	{
		this.fats = fats;
	}

	public double getCarbs()
	{
		return carbs;
	}

	public void setCarbs(double carbs)
	{
		this.carbs = carbs;
	}

	public int getBloodOutTime()
	{
		return bloodOutTime;
	}

	public void setBloodOutTime(int bloodOutTime)
	{
		this.bloodOutTime = bloodOutTime;
	}

	public double getBloodOutValue()
	{
		return bloodOutValue;
	}

	public void setBloodOutValue(double bloodOutValue)
	{
		this.bloodOutValue = bloodOutValue;
	}
}
