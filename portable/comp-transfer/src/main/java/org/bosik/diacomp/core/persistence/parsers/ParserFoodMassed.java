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
import org.bosik.diacomp.core.utils.Utils;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserFoodMassed extends Parser<FoodMassed>
{
	@Override
	public FoodMassed read(JSONObject json) throws JSONException
	{
		FoodMassed item = new FoodMassed();

		item.setName(json.getString("name"));
		item.setRelProts(json.getDouble("prots"));
		item.setRelFats(json.getDouble("fats"));
		item.setRelCarbs(json.getDouble("carbs"));
		item.setRelValue(json.getDouble("value"));
		item.setMass(json.getDouble("mass"));

		return item;
	}

	@Override
	public JSONObject write(FoodMassed object) throws JSONException
	{
		JSONObject json = new JSONObject();

		json.put("name", object.getName());
		json.put("prots", Utils.round2(object.getRelProts()));
		json.put("fats", Utils.round2(object.getRelFats()));
		json.put("carbs", Utils.round2(object.getRelCarbs()));
		json.put("value", Utils.round2(object.getRelValue()));
		json.put("mass", Utils.round2(object.getMass()));

		return json;
	}
}
