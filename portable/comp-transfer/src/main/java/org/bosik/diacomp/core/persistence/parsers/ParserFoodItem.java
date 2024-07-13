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

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserFoodItem extends Parser<FoodItem>
{
	@Override
	public FoodItem read(JSONObject json) throws JSONException
	{
		FoodItem item = new FoodItem();

		item.setName(json.getString(FoodItem.FIELD_NAME));
		item.setRelProts(json.getDouble(FoodItem.FIELD_PROTS));
		item.setRelFats(json.getDouble(FoodItem.FIELD_FATS));
		item.setRelCarbs(json.getDouble(FoodItem.FIELD_CARBS));
		item.setRelValue(json.getDouble(FoodItem.FIELD_VALUE));
		item.setTag(json.has(FoodItem.FIELD_TAG) ? json.getInt(FoodItem.FIELD_TAG) : 0);
		item.setFromTable(json.getBoolean(FoodItem.FIELD_TABLE));

		return item;
	}

	@Override
	public JSONObject write(FoodItem object) throws JSONException
	{
		JSONObject json = new JSONObject();

		json.put(FoodItem.FIELD_NAME, object.getName());
		json.put(FoodItem.FIELD_PROTS, object.getRelProts());
		json.put(FoodItem.FIELD_FATS, object.getRelFats());
		json.put(FoodItem.FIELD_CARBS, object.getRelCarbs());
		json.put(FoodItem.FIELD_VALUE, object.getRelValue());
		json.put(FoodItem.FIELD_TAG, object.getTag());
		json.put(FoodItem.FIELD_TABLE, object.getFromTable());

		return json;
	}
}
