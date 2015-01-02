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
