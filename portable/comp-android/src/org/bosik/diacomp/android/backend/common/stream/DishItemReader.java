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

import java.io.IOException;
import java.util.List;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import android.util.JsonReader;

public class DishItemReader extends StreamReader<DishItem>
{
	private final FoodMassedReader foodMassedReader = new FoodMassedReader();

	@Override
	public DishItem read(JsonReader json) throws IOException
	{
		DishItem item = new DishItem();

		json.beginObject();
		while (json.hasNext())
		{
			String name = json.nextName();

			switch (name)
			{
				case "name":
				{
					item.setName(json.nextString());
					break;
				}
				case "tag":
				{
					item.setTag(json.nextInt());
					break;
				}
				case "mass":
				{
					item.setMass(json.nextDouble());
					break;
				}
				case "content":
				{
					List<FoodMassed> items = foodMassedReader.readAll(json);
					for (FoodMassed f : items)
					{
						item.add(f);
					}
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