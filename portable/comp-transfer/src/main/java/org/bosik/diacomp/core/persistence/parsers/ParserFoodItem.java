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
		//return new Gson().fromJson(json.toString(), FoodItem.class);

		FoodItem item = new FoodItem();

		item.setName(json.getString("name"));
		item.setRelProts(json.getDouble("prots"));
		item.setRelFats(json.getDouble("fats"));
		item.setRelCarbs(json.getDouble("carbs"));
		item.setRelValue(json.getDouble("value"));
		item.setTag(json.getInt("tag"));
		item.setFromTable(json.getBoolean("table"));

		return item;
	}

	@Override
	public JSONObject write(FoodItem object) throws JSONException
	{
		//return new JSONObject(new Gson().toJson(item));

		JSONObject json = new JSONObject();

		json.put("name", object.getName());
		json.put("prots", object.getRelProts());
		json.put("fats", object.getRelFats());
		json.put("carbs", object.getRelCarbs());
		json.put("value", object.getRelValue());
		json.put("tag", object.getTag());
		json.put("table", object.getFromTable());

		return json;
	}
}
