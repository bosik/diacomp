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

import org.bosik.diacomp.core.entities.business.foodbase.FoodCommon;
import org.bosik.diacomp.core.utils.Utils;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserFoodCommon extends Parser<FoodCommon>
{
	private static final String FIELD_ID            = "id";
	private static final String FIELD_NAME          = "name";
	private static final String FIELD_PROTS         = "prots";
	private static final String FIELD_FATS          = "fats";
	private static final String FIELD_CARBS         = "carbs";
	private static final String FIELD_VALUE         = "value";
	private static final String FIELD_DELETED       = "deleted";
	private static final String FIELD_LAST_MODIFIED = "lastModified";
	private static final String FIELD_TAG           = "tag";

	@Override
	public FoodCommon read(JSONObject json) throws JSONException
	{
		FoodCommon item = new FoodCommon();

		item.setId(json.getString(FIELD_ID));
		item.setName(json.getString(FIELD_NAME));
		item.setRelProts(json.getDouble(FIELD_PROTS));
		item.setRelFats(json.getDouble(FIELD_FATS));
		item.setRelCarbs(json.getDouble(FIELD_CARBS));
		item.setRelValue(json.getDouble(FIELD_VALUE));
		item.setDeleted(json.getBoolean(FIELD_DELETED));
		item.setLastModified(Utils.parseTimeUTC(json.getString(FIELD_LAST_MODIFIED)));
		item.setTag(json.getString(FIELD_TAG));

		return item;
	}

	@Override
	public JSONObject write(FoodCommon object) throws JSONException
	{
		JSONObject json = new JSONObject();

		json.put(FIELD_ID, object.getId());
		json.put(FIELD_NAME, object.getName());
		json.put(FIELD_PROTS, object.getRelProts());
		json.put(FIELD_FATS, object.getRelFats());
		json.put(FIELD_CARBS, object.getRelCarbs());
		json.put(FIELD_VALUE, object.getRelValue());
		json.put(FIELD_DELETED, object.isDeleted());
		json.put(FIELD_LAST_MODIFIED, Utils.formatTimeUTC(object.getLastModified()));
		json.put(FIELD_TAG, object.getTag());

		return json;
	}
}
