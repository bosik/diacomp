package org.bosik.diacomp.core.entities.business.foodbase;

import java.util.Locale;
import org.bosik.diacomp.core.entities.business.Food;
import org.bosik.diacomp.core.entities.business.interfaces.NamedRelativeTagged;
import com.google.gson.annotations.SerializedName;

/**
 * Food item for food base
 * 
 * @author Bosik
 * 
 */
public class FoodItem extends Food implements NamedRelativeTagged
{
	private static final long	serialVersionUID	= 1789285539891342521L;

	@SerializedName("tag")
	private int					tag;
	@SerializedName("table")
	private boolean				fromTable;

	public FoodItem()
	{

	}

	public FoodItem(String name, double relProts, double relFats, double relCarbs, double relValue, int tag,
			boolean fromTable)
	{
		super(name, relProts, relFats, relCarbs, relValue);
		setTag(tag);
		setFromTable(fromTable);
	}

	public FoodItem(FoodItem food)
	{
		super(food.getName(), food.getRelProts(), food.getRelFats(), food.getRelCarbs(), food.getRelValue());
		setTag(food.getTag());
		setFromTable(food.getFromTable());
	}

	// ================================ GET / SET ================================

	@Override
	public int getTag()
	{
		return tag;
	}

	@Override
	public void setTag(int tag)
	{
		this.tag = tag;
	}

	public boolean getFromTable()
	{
		return fromTable;
	}

	public void setFromTable(boolean fromTable)
	{
		this.fromTable = fromTable;
	}

	// ================================ MISC ================================

	@Override
	public String toString()
	{
		return String.format(Locale.US, "%s[%.1f|%.1f|%.1f|%.1f]:%s", getName(), getRelProts(), getRelFats(),
				getRelCarbs(), getRelValue(), getFromTable());
	}
}
