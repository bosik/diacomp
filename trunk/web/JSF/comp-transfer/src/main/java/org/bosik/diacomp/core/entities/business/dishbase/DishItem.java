package org.bosik.diacomp.core.entities.business.dishbase;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.business.interfaces.NamedRelativeTagged;
import org.bosik.diacomp.core.utils.Utils;
import com.google.gson.annotations.SerializedName;

/**
 * Note: no check (rel* < 100) is presented
 */

public class DishItem implements NamedRelativeTagged, Serializable
{
	private static final long		serialVersionUID	= 1L;

	@SerializedName("name")
	private String					name;

	@SerializedName("tag")
	private int						tag;

	@SerializedName("mass")
	private Double					mass;

	private final List<FoodMassed>	content				= new ArrayList<FoodMassed>();

	// ================================ GET / SET ================================

	private double getRealMass()
	{
		if (mass != null)
		{
			return mass;
		}

		double result = 0.0;
		for (FoodMassed item : content)
		{
			result += item.getMass();
		}
		return result;
	}

	public Double getMass()
	{
		return mass;
	}

	public void setMass(Double mass)
	{
		if (mass == null)
		{
			this.mass = mass;
		}
		else
		{
			setMass((double)mass);
		}
	}

	public void setMass(double mass)
	{
		if (mass > Utils.EPS)
		{
			this.mass = mass;
		}
		else
		{
			throw new IllegalArgumentException(String.format("Incorrect mass: %f", mass));
		}
	}

	@Override
	public String getName()
	{
		return name;
	}

	public void setName(String name)
	{
		this.name = name;
	}

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

	private double getRel(double total)
	{
		double realMass = getRealMass();
		if (realMass > Utils.EPS)
		{
			return total / realMass;
		}
		else
		{
			return 0.0;
		}
	}

	@Override
	public double getRelProts()
	{
		double total = 0.0;
		for (FoodMassed item : content)
		{
			total += item.getProts();
		}

		return getRel(total);
	}

	@Override
	public double getRelFats()
	{
		double result = 0.0;
		for (FoodMassed item : content)
		{
			result += item.getFats();
		}

		return getRel(result);
	}

	@Override
	public double getRelCarbs()
	{
		double total = 0.0;
		for (FoodMassed item : content)
		{
			total += item.getCarbs();
		}

		return getRel(total);
	}

	@Override
	public double getRelValue()
	{
		double total = 0.0;
		for (FoodMassed item : content)
		{
			total += item.getValue();
		}

		return getRel(total);
	}

	// =================================== LIST METHODS ===================================

	public void add(FoodMassed item)
	{
		if (item != null)
		{
			content.add(item);
		}
		else
		{
			throw new NullPointerException("Dish item can't be null");
		}
	}

	public void clear()
	{
		content.clear();
	}

	public void remove(int index)
	{
		content.remove(index);
	}

	public int count()
	{
		return content.size();
	}

	public FoodMassed get(int index)
	{
		return content.get(index);
	}

	// =================================== OTHER ===================================

	public FoodItem convertToFood()
	{
		FoodItem food = new FoodItem();

		food.setName(getName());
		food.setRelProts(getRelProts());
		food.setRelFats(getRelFats());
		food.setRelCarbs(getRelCarbs());
		food.setRelValue(getRelValue());
		food.setFromTable(false);
		food.setTag(getTag());

		return food;
	}
}
