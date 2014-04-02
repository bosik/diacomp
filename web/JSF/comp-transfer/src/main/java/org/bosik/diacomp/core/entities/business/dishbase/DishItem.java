package org.bosik.diacomp.core.entities.business.dishbase;

import java.io.Serializable;
import java.util.LinkedList;
import java.util.List;
import org.bosik.diacomp.core.entities.business.FoodMassed;
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

	private final List<FoodMassed>	content				= new LinkedList<FoodMassed>();

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

	@Override
	public double getRelProts()
	{
		double result = 0.0;
		for (FoodMassed item : content)
		{
			result += item.getProts();
		}

		// TODO: implement zero-check
		return result / getRealMass();
	}

	@Override
	public double getRelFats()
	{
		double result = 0.0;
		for (FoodMassed item : content)
		{
			result += item.getFats();
		}

		// TODO: implement zero-check
		return result / getRealMass();
	}

	@Override
	public double getRelCarbs()
	{
		double result = 0.0;
		for (FoodMassed item : content)
		{
			result += item.getCarbs();
		}

		// TODO: implement zero-check
		return result / getRealMass();
	}

	@Override
	public double getRelValue()
	{
		double result = 0.0;
		for (FoodMassed item : content)
		{
			result += item.getValue();
		}

		// TODO: implement zero-check
		return result / getRealMass();
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
}
