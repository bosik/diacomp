package org.bosik.compensation.bo.dishbase;

import java.io.Serializable;
import org.bosik.compensation.bo.basic.Named;
import org.bosik.compensation.bo.basic.TrueCloneable;
import org.bosik.compensation.persistence.dao.BaseItem;

public class DishItem implements BaseItem, TrueCloneable, Serializable
{
	private static final long	serialVersionUID	= 1L;

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

	@Override
	public int getTag()
	{
		// TODO Auto-generated method stub
		return 0;
	}

	public void setTag(int tag)
	{
		// TODO Auto-generated method stub
	}

	// ================================ CLONE ================================

	@Override
	public Named clone() throws CloneNotSupportedException
	{
		DishItem result = (DishItem) super.clone();

		// TODO: implement

		return result;
	}

	@Override
	public String getName()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getId()
	{
		// TODO Auto-generated method stub
		return null;
	}
}
