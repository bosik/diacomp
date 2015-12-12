/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.core.persistence.serializers;

import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.List;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.utils.Utils;

public class SerializerFoodMassedPlain implements Serializer<FoodMassed>
{
	private static final DecimalFormat	df			= new DecimalFormat("###.#");
	private static final char			FOOD_SEP	= '|';

	@Override
	public FoodMassed read(String data)
	{
		String[] t = data.split("[\\[" + FOOD_SEP + "\\]:]+");

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

	@Override
	public List<FoodMassed> readAll(String data)
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public String write(FoodMassed food)
	{
		return String.format("%s[%s" + FOOD_SEP + "%s" + FOOD_SEP + "%s" + FOOD_SEP + "%s]:%s", food.getName(),
				df.format(food.getRelProts()), df.format(food.getRelFats()), df.format(food.getRelCarbs()),
				df.format(food.getRelValue()), df.format(food.getMass()));
	}

	@Override
	public String writeAll(Iterable<FoodMassed> objects)
	{
		throw new UnsupportedOperationException("Not implemented");
	}
}
