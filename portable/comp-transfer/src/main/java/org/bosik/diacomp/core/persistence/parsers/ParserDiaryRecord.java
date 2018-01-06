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
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import org.bosik.diacomp.core.utils.Utils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.List;

public class ParserDiaryRecord extends Parser<DiaryRecord>
{
	private static final String FIELD_TYPE = "type";
	private static final String FIELD_TIME = "time";

	private static final String TYPE_BLOOD = "blood";
	private static final String TYPE_INS   = "ins";
	private static final String TYPE_MEAL  = "meal";
	private static final String TYPE_NOTE  = "note";

	private static final String FIELD_BLOOD_VALUE  = "value";
	private static final String FIELD_BLOOD_FINGER = "finger";
	private static final String FIELD_INS_VALUE    = "value";
	private static final String FIELD_MEAL_SHORT   = "short";
	private static final String FIELD_MEAL_CONTENT = "content";
	private static final String FIELD_NOTE_TEXT    = "text";

	private final Parser<FoodMassed> parserFoodMassed = new ParserFoodMassed();

	@Override
	public DiaryRecord read(JSONObject json) throws JSONException
	{
		String type = json.getString(FIELD_TYPE);

		switch (type)
		{
			case TYPE_BLOOD:
			{
				BloodRecord item = new BloodRecord();
				item.setTime(Utils.parseTimeUTC(json.getString(FIELD_TIME)));
				item.setValue(json.getDouble(FIELD_BLOOD_VALUE));
				item.setFinger(json.getInt(FIELD_BLOOD_FINGER));
				return item;
			}

			case TYPE_INS:
			{
				InsRecord item = new InsRecord();
				item.setTime(Utils.parseTimeUTC(json.getString(FIELD_TIME)));
				item.setValue(json.getDouble(FIELD_INS_VALUE));
				return item;
			}

			case TYPE_MEAL:
			{
				MealRecord item = new MealRecord();
				item.setTime(Utils.parseTimeUTC(json.getString(FIELD_TIME)));
				item.setShortMeal(json.getBoolean(FIELD_MEAL_SHORT));
				List<FoodMassed> items = parserFoodMassed.readAll(json.getJSONArray(FIELD_MEAL_CONTENT));

				for (FoodMassed f : items)
				{
					item.add(f);
				}

				return item;
			}

			case TYPE_NOTE:
			{
				NoteRecord item = new NoteRecord();
				item.setTime(Utils.parseTimeUTC(json.getString(FIELD_TIME)));
				item.setText(json.getString(FIELD_NOTE_TEXT));
				return item;
			}

			default:
			{
				throw new UnsupportedOperationException("Unknown record type: '" + type + "'");
			}
		}
	}

	@Override
	public JSONObject write(DiaryRecord object) throws JSONException
	{
		JSONObject json = new JSONObject();

		json.put(FIELD_TIME, Utils.formatTimeUTC(object.getTime()));

		if (object.getClass() == BloodRecord.class)
		{
			BloodRecord item = (BloodRecord) object;
			json.put(FIELD_TYPE, TYPE_BLOOD);
			json.put(FIELD_BLOOD_VALUE, item.getValue());
			json.put(FIELD_BLOOD_FINGER, item.getFinger());
		}
		else if (object.getClass() == InsRecord.class)
		{
			InsRecord item = (InsRecord) object;
			json.put(FIELD_TYPE, TYPE_INS);
			json.put(FIELD_INS_VALUE, item.getValue());
		}
		else if (object.getClass() == MealRecord.class)
		{
			MealRecord item = (MealRecord) object;
			json.put(FIELD_TYPE, TYPE_MEAL);
			json.put(FIELD_MEAL_SHORT, item.getShortMeal());

			JSONArray foods = new JSONArray();
			for (int i = 0; i < item.count(); i++)
			{
				foods.put(parserFoodMassed.write(item.get(i)));
			}
			json.put(FIELD_MEAL_CONTENT, foods);
		}
		else if (object.getClass() == NoteRecord.class)
		{
			NoteRecord item = (NoteRecord) object;
			json.put(FIELD_TYPE, TYPE_NOTE);
			json.put(FIELD_NOTE_TEXT, item.getText());
		}
		else
		{
			throw new UnsupportedOperationException("Unknown record type: " + object.getClass().getName());
		}

		return json;
	}
}
