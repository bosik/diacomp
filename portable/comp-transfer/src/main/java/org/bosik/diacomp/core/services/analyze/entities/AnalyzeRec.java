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

public class AnalyzeRec
{
	private double	prots;
	private double	fats;
	private double	carbs;
	private double	ins;
	private double	bsIn;
	private double	bsOut;
	private int		time;
	private double	weight;

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

	public double getIns()
	{
		return ins;
	}

	public void setIns(double ins)
	{
		this.ins = ins;
	}

	public double getBsIn()
	{
		return bsIn;
	}

	public void setBsIn(double bsIn)
	{
		this.bsIn = bsIn;
	}

	public double getBsOut()
	{
		return bsOut;
	}

	public void setBsOut(double bsOut)
	{
		this.bsOut = bsOut;
	}

	public int getTime()
	{
		return time;
	}

	public void setTime(int time)
	{
		this.time = time;
	}

	public double getWeight()
	{
		return weight;
	}

	public void setWeight(double weight)
	{
		this.weight = weight;
	}
}
