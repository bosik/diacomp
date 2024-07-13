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

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;

import java.io.IOException;

public class FoodItemReader extends StreamReader<FoodItem>
{
	@Override
	public FoodItem read(JsonReader json) throws IOException
	{
		FoodItem item = new FoodItem();

		json.beginObject();
		while (json.hasNext())
		{
			String name = json.nextName();

			switch (name)
			{
				case FoodItem.FIELD_NAME:
				{
					item.setName(json.nextString());
					break;
				}
				case FoodItem.FIELD_PROTS:
				{
					item.setRelProts(json.nextDouble());
					break;
				}
				case FoodItem.FIELD_FATS:
				{
					item.setRelFats(json.nextDouble());
					break;
				}
				case FoodItem.FIELD_CARBS:
				{
					item.setRelCarbs(json.nextDouble());
					break;
				}
				case FoodItem.FIELD_VALUE:
				{
					item.setRelValue(json.nextDouble());
					break;
				}
				case FoodItem.FIELD_TAG:
				{
					item.setTag(json.nextInt());
					break;
				}
				case FoodItem.FIELD_TABLE:
				{
					item.setFromTable(json.nextBoolean());
					break;
				}
				default:
				{
					throw new IllegalArgumentException("Unexpected property: " + name);
				}
			}
		}
		json.endObject();

		return item;
	}
}