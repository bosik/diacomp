package org.bosik.compensation.bo.dishbase;

import org.bosik.compensation.bo.IRelative;
import org.bosik.compensation.bo.basic.UniqueNamed;

public class DishItem extends UniqueNamed implements IRelative
{
	private static final long	serialVersionUID	= 1L;

	public DishItem(String name)
	{
		super(name);
	}

	// ================================ GET / SET ================================

	@Override
	public double getRelProts()
	{
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public double getRelFats()
	{
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public double getRelCarbs()
	{
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public double getRelValue()
	{
		// TODO Auto-generated method stub
		return 0;
	}

	// ================================ CLONE ================================

	@Override
	public UniqueNamed clone() throws CloneNotSupportedException
	{
		DishItem result = (DishItem) super.clone();

		// TODO: implement

		return result;
	}
}
