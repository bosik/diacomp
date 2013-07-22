package org.bosik.compensation.persistence.entity.common;

/**
 * Хранит имя и относительные параметры (на 100 г)
 * 
 * @author Bosik
 * 
 */
public class FoodData extends Item
{
	private String name;
	private double relProts;
	private double relFats;
	private double relCarbs;
	private double relValue;

	// public FoodData()
	// {
	// super();
	// THINK: will super constructor be invoked without super(); ?
	// }

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

	// ================================ БРОСАТЕЛИ ================================

	protected static void checkAndThrow(boolean check, String errorMessage)
	{
		if (!check)
			throw new IllegalArgumentException(errorMessage);
	}

	protected static void checkNameThrowable(String name)
	{
		checkAndThrow(checkName(name), "Name can't be null or empty (" + name + ")");
	}

	protected static void checkRelThrowable(double value)
	{
		checkAndThrow(checkRelativeValue(value), "Relative value is out of [0, 100] bounds (" + String.valueOf(value)
				+ ")");
	}

	protected static void checkNonNegativeThrowable(double value)
	{
		checkAndThrow(checkNonNegativeValue(value), "Value can't be negative (" + String.valueOf(value) + ")");
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
	public Item clone() throws CloneNotSupportedException
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
