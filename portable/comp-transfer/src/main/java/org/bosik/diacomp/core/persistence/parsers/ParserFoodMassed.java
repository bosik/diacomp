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
	private static final String FIELD_NAME  = "name";
	private static final String FIELD_PROTS = "prots";
	private static final String FIELD_FATS  = "fats";
	private static final String FIELD_CARBS = "carbs";
	private static final String FIELD_VALUE = "value";
	private static final String FIELD_MASS  = "mass";

	@Override
	public FoodMassed read(JSONObject json) throws JSONException
	{
		FoodMassed item = new FoodMassed();

		item.setName(json.getString(FIELD_NAME));
		item.setRelProts(json.getDouble(FIELD_PROTS));
		item.setRelFats(json.getDouble(FIELD_FATS));
		item.setRelCarbs(json.getDouble(FIELD_CARBS));
		item.setRelValue(json.getDouble(FIELD_VALUE));
		item.setMass(json.getDouble(FIELD_MASS));

		return item;
	}

	@Override
	public JSONObject write(FoodMassed object) throws JSONException
	{
		JSONObject json = new JSONObject();

		json.put(FIELD_NAME, object.getName());
		json.put(FIELD_PROTS, Utils.round2(object.getRelProts()));
		json.put(FIELD_FATS, Utils.round2(object.getRelFats()));
		json.put(FIELD_CARBS, Utils.round2(object.getRelCarbs()));
		json.put(FIELD_VALUE, Utils.round2(object.getRelValue()));
		json.put(FIELD_MASS, Utils.round2(object.getMass()));

		return json;
	}
}
