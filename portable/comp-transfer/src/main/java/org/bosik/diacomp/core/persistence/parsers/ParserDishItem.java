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
package org.bosik.diacomp.core.persistence.parsers;

import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserDishItem extends Parser<DishItem>
{
	private static final String FIELD_NAME    = "name";
	private static final String FIELD_TAG     = "tag";
	private static final String FIELD_MASS    = "mass";
	private static final String FIELD_CONTENT = "content";

	private final Parser<FoodMassed> parserFoodMassed = new ParserFoodMassed();

	@Override
	public DishItem read(JSONObject json) throws JSONException
	{
		DishItem dish = new DishItem();
		dish.setName(json.getString(FIELD_NAME));
		dish.setTag(json.has(FIELD_TAG) ? json.getInt(FIELD_TAG) : 0);
		dish.setMass(json.has(FIELD_MASS) ? json.getDouble(FIELD_MASS) : null);

		if (json.has(FIELD_CONTENT))
		{
			JSONArray content = json.getJSONArray(FIELD_CONTENT);
			for (int i = 0; i < content.length(); i++)
			{
				JSONObject item = content.getJSONObject(i);
				FoodMassed foodMassed = parserFoodMassed.read(item);
				dish.add(foodMassed);
			}
		}

		return dish;
	}

	@Override
	public JSONObject write(DishItem item) throws JSONException
	{
		JSONObject json = new JSONObject();

		json.put(FIELD_NAME, item.getName());
		json.put(FIELD_TAG, item.getTag());

		if (item.getMass() != null)
		{
			json.put(FIELD_MASS, item.getMass());
		}

		JSONArray content = new JSONArray();
		for (int i = 0; i < item.count(); i++)
		{
			content.put(parserFoodMassed.write(item.get(i)));
		}

		json.put(FIELD_CONTENT, content);

		return json;
	}
}
