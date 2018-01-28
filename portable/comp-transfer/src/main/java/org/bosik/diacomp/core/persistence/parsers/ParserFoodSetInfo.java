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

import org.bosik.diacomp.core.entities.business.FoodSetInfo;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserFoodSetInfo extends Parser<FoodSetInfo>
{
	private static final String FIELD_ID          = "id";
	private static final String FIELD_DESCRIPTION = "description";
	private static final String FIELD_SIZE        = "size";

	@Override
	public FoodSetInfo read(JSONObject json) throws JSONException
	{
		FoodSetInfo item = new FoodSetInfo();

		item.setId(json.getString(FIELD_ID));
		item.setDescription(json.getString(FIELD_DESCRIPTION));
		item.setSize(json.getInt(FIELD_SIZE));

		return item;
	}

	@Override
	public JSONObject write(FoodSetInfo object) throws JSONException
	{
		JSONObject json = new JSONObject();

		json.put(FIELD_ID, object.getId());
		json.put(FIELD_DESCRIPTION, object.getDescription());
		json.put(FIELD_SIZE, object.getSize());

		return json;
	}
}
