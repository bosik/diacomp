package org.bosik.compensation.bo.foodbase;

import java.util.Locale;
import org.bosik.compensation.bo.Food;
import org.bosik.compensation.bo.basic.Named;
import org.bosik.compensation.bo.basic.TrueCloneable;
import org.bosik.compensation.persistence.dao.BaseItem;

/**
 * Food item for food base
 * 
 * @author Bosik
 * 
 */
public class FoodItem extends Food implements TrueCloneable, BaseItem
{
	private static final long	serialVersionUID	= 1789285539891342521L;

	private String				id;
	private boolean				fromTable;

	// ================================ GET / SET ================================

	@Override
	public String getId()
	{
		return id;
	}

	public void setId(String id)
	{
		this.id = id;
	}

	public boolean getFromTable()
	{
		return fromTable;
	}

	public void setFromTable(boolean fromTable)
	{
		this.fromTable = fromTable;
	}

	// ================================ CLONE ================================

	@Override
	public Named clone() throws CloneNotSupportedException
	{
		FoodItem result = (FoodItem) super.clone();

		result.setFromTable(getFromTable());
		result.setTag(getTag());

		return result;
	}

	@Override
	public String toString()
	{
		return String.format(Locale.US, "%s[%.1f|%.1f|%.1f|%.1f]:%s", getName(), getRelProts(), getRelFats(),
				getRelCarbs(), getRelValue(), getFromTable());
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
}
