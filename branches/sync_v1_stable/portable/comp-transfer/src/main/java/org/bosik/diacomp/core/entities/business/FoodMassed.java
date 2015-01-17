package org.bosik.diacomp.core.entities.business;

/**
 * Stores food's name, relative parameters (PFCV on 100g) and mass.
 *
 * @author Bosik
 *
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
