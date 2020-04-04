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
	private static final String FIELD_NAME  = "name";
	private static final String FIELD_PROTS = "prots";
	private static final String FIELD_FATS  = "fats";
	private static final String FIELD_CARBS = "carbs";
	private static final String FIELD_VALUE = "value";
	private static final String FIELD_TAG   = "tag";
	private static final String FIELD_TABLE = "table";

	@Override
	public FoodItem read(JSONObject json) throws JSONException
	{
		FoodItem item = new FoodItem();

		item.setName(json.getString(FIELD_NAME));
		item.setRelProts(json.getDouble(FIELD_PROTS));
		item.setRelFats(json.getDouble(FIELD_FATS));
		item.setRelCarbs(json.getDouble(FIELD_CARBS));
		item.setRelValue(json.getDouble(FIELD_VALUE));
		item.setTag(json.has(FIELD_TAG) ? json.getInt(FIELD_TAG) : 0);
		item.setFromTable(json.getBoolean(FIELD_TABLE));

		return item;
	}

	@Override
	public JSONObject write(FoodItem object) throws JSONException
	{
		JSONObject json = new JSONObject();

		json.put(FIELD_NAME, object.getName());
		json.put(FIELD_PROTS, object.getRelProts());
		json.put(FIELD_FATS, object.getRelFats());
		json.put(FIELD_CARBS, object.getRelCarbs());
		json.put(FIELD_VALUE, object.getRelValue());
		json.put(FIELD_TAG, object.getTag());
		json.put(FIELD_TABLE, object.getFromTable());

		return json;
	}
}
