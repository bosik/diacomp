package org.bosik.compensation.bo;

import org.bosik.compensation.bo.basic.UniqueNamed;

public abstract class RelativeTagged extends UniqueNamed
{
	private static final long	serialVersionUID	= 1L;
	private int					tag;

	public RelativeTagged(String name)
	{
		super(name);
	}
	
	public int getTag()
	{
		return tag;
	}

	public void setTag(int tag)
	{
		this.tag = tag;
	}

	abstract public double getRelProts();

	abstract public double getRelFats();

	abstract public double getRelCarbs();

	abstract public double getRelValue();
}