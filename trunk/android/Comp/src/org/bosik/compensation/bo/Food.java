package org.bosik.compensation.bo;

import org.bosik.compensation.bo.basic.UniqueNamed;

/**
 * Stores food's name and relative parameters (PFCV on 100g)
 * 
 * @author Bosik
 * 
 */
public class Food extends UniqueNamed implements IRelative
{
	private static final long	serialVersionUID	= 1L;

	private double				relProts;
	private double				relFats;
	private double				relCarbs;
	private double				relValue;

	public Food(String name)
	{
		super(name);
	}

	public Food(String name, double relProts, double relFats, double relCarbs, double relValue)
	{
		super(name);
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
		checkAndThrow(checkRelativeValue(value), String.format("Relative value (%s) is out of [0, 100] bounds", value));
	}

	protected static void checkNonNegativeThrowable(double value)
	{
		checkAndThrow(checkNonNegativeValue(value), String.format("Value can't be negative (%s)", value));
	}

	// ================================ GET / SET ================================

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

	// ================================ CLONE ================================

	@Override
	public UniqueNamed clone() throws CloneNotSupportedException
	{
		Food result = (Food) super.clone();

		result.setName(getName());
		result.setRelCarbs(getRelCarbs());
		result.setRelFats(getRelFats());
		result.setRelProts(getRelProts());
		result.setRelValue(getRelValue());

		return result;
	}
}
