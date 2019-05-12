/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package org.bosik.diacomp.android.backend.common.stream;

import android.util.JsonReader;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import org.bosik.diacomp.core.utils.Utils;

import java.io.IOException;
import java.util.Date;
import java.util.List;

public class DiaryRecordReader extends StreamReader<DiaryRecord>
{
	private final FoodMassedReader foodMassedReader = new FoodMassedReader();

	@Override
	public DiaryRecord read(JsonReader json) throws IOException
	{
		// TODO: very dirty, refactor once possible

		BloodRecord bloodRecord = new BloodRecord();
		InsRecord insRecord = new InsRecord();
		MealRecord mealRecord = new MealRecord();
		NoteRecord noteRecord = new NoteRecord();

		String type = null;

		json.beginObject();

		while (json.hasNext())
		{
			String name = json.nextName();

			switch (name)
			{
				case "type":
				{
					type = json.nextString();
					break;
				}
				case "time":
				{
					Date time = Utils.parseTimeUTC(json.nextString());

					bloodRecord.setTime(time);
					insRecord.setTime(time);
					mealRecord.setTime(time);
					noteRecord.setTime(time);
					break;
				}
				case "value":
				{
					double value = json.nextDouble();

					bloodRecord.setValue(value);
					insRecord.setValue(value);
					break;
				}
				case "finger":
				{
					bloodRecord.setFinger(json.nextInt());
					break;
				}
				case "short":
				{
					mealRecord.setShortMeal(json.nextBoolean());
					break;
				}
				case "content":
				{
					List<FoodMassed> items = foodMassedReader.readAll(json);
					for (FoodMassed f : items)
					{
						mealRecord.add(f);
					}
					break;
				}
				case "text":
				{
					noteRecord.setText(json.nextString());
					break;
				}
				default:
				{
					throw new IllegalArgumentException("Unexpected property: " + name);
				}
			}
		}

		json.endObject();

		if (type != null)
		{
			switch (type)
			{
				case "blood":
					return bloodRecord;
				case "ins":
					return insRecord;
				case "meal":
					return mealRecord;
				case "note":
					return noteRecord;
				default:
					throw new UnsupportedOperationException("Unknown record type: '" + type + "'");
			}
		}
		else
		{
			throw new IllegalArgumentException("Missing record type");
		}
	}
}
