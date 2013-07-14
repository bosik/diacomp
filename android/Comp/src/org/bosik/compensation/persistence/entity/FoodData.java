package org.bosik.compensation.persistence.entity;

public class FoodData extends CustomItem
{
	private String name;
	private double relProts;
	private double relFats;
	private double relCarbs;
	private double relValue;

	// ================================ ВАЛИДАТОРЫ ================================

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

	private static void checkNameThrowable(String name)
	{
		if (!checkName(name))
			throw new IllegalArgumentException("Name can't be null or empty (" + name + ")");
	}

	private static void checkRelThrowable(double value)
	{
		if (!checkRelativeValue(value))
			throw new IllegalArgumentException("Relative value is out of [0, 100] bounds (" + String.valueOf(value) + ")");
	}

	protected static void checkNonNegativeThrowable(double value)
	{
		if (!checkNonNegativeValue(value))
			throw new IllegalArgumentException("Value can't be negative (" + String.valueOf(value) + ")");
	}

	// ================================ GET / SET ================================

	public String getName()
	{
		return name;
	}

	public double getRelProts()
	{
		return relProts;
	}

	public double getRelFats()
	{
		return relFats;
	}

	public double getRelCarbs()
	{
		return relCarbs;
	}

	public double getRelValue()
	{
		return relValue;
	}

	public void setName(String name)
	{
		checkNameThrowable(name);
		this.name = name;
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
	public CustomItem clone() throws CloneNotSupportedException
	{
		FoodData result = (FoodData) super.clone();
		
		result.setName(getName());
		result.setRelCarbs(getRelCarbs());
		result.setRelFats(getRelFats());
		result.setRelProts(getRelProts());
		result.setRelValue(getRelValue());

		return result;
	}
}
