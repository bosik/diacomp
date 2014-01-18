package org.bosik.diacomp.persistence.serializers;

import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.List;
import org.bosik.diacomp.bo.FoodMassed;
import org.bosik.diacomp.utils.Utils;

public class SerializerFoodMassedPlain implements Serializer<FoodMassed>
{
	// private static final String TAG = SerializerFoodMassedPlain.class.getSimpleName();
	private static final DecimalFormat	df			= new DecimalFormat("###.#");
	private static final char			FOOD_SEP	= '|';

	public FoodMassed read(String data)
	{
		String[] t = data.split("[\\[" + FOOD_SEP + "\\]:]+"); // БОЯН :D

		if (t.length != 6)
		{
			throw new IllegalArgumentException("Incorrect FoodMassed format: " + data);
		}

		try
		{
			FoodMassed food = new FoodMassed();

			// setters are used for additional validating
			food.setName(t[0]);
			food.setRelProts(Utils.parseDouble(t[1]));
			food.setRelFats(Utils.parseDouble(t[2]));
			food.setRelCarbs(Utils.parseDouble(t[3]));
			food.setRelValue(Utils.parseDouble(t[4]));
			food.setMass(Utils.parseDouble(t[5]));

			return food;
		}
		catch (ParseException e)
		{
			throw new IllegalArgumentException("Incorrect FoodMassed format: " + data);
		}
	}

	public List<FoodMassed> readAll(String data)
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	public String write(FoodMassed food)
	{
		return String.format("%s[%s" + FOOD_SEP + "%s" + FOOD_SEP + "%s" + FOOD_SEP + "%s]:%s", food.getName(),
				df.format(food.getRelProts()), df.format(food.getRelFats()), df.format(food.getRelCarbs()),
				df.format(food.getRelValue()), df.format(food.getMass()));
	}

	public String writeAll(List<FoodMassed> objects)
	{
		throw new UnsupportedOperationException("Not implemented");
	}
}
