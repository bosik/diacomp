package org.bosik.compensation.bo.dishbase;

import java.io.Serializable;
import org.bosik.compensation.bo.RelativeTagged;

public class DishItem implements RelativeTagged, Serializable
{
	private static final long	serialVersionUID	= 1L;

	// ================================ GET / SET ================================

	@Override
	public String getName()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int getTag()
	{
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void setTag(int tag)
	{
		// TODO Auto-generated method stub

	}

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
}
