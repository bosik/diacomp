package org.bosik.diacomp.bo.foodbase;

import java.io.Serializable;
import java.util.Locale;
import org.bosik.diacomp.bo.Food;
import org.bosik.diacomp.bo.basic.RelativeTagged;
import com.google.gson.annotations.SerializedName;

/**
 * Food item for food base
 * 
 * @author Bosik
 * 
 */
public class FoodItem extends Food implements Serializable, RelativeTagged
{
	private static final long	serialVersionUID	= 1789285539891342521L;

	@SerializedName("tag")
	private int					tag;
	@SerializedName("table")
	private boolean				fromTable;

	// ================================ GET / SET ================================

	public int getTag()
	{
		return tag;
	}

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
