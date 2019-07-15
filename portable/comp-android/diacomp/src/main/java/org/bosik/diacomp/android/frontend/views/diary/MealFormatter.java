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
package org.bosik.diacomp.android.frontend.views.diary;

import android.content.Context;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.services.diary.MealFormat;
import org.bosik.diacomp.core.utils.Utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;

public final class MealFormatter
{
	public static String format(MealRecord meal, Context context, MealFormat style)
	{
		switch (style)
		{
			case SINGLE_MOST_CARBS:
			{
				double max = -1;
				String name = "";
				for (int i = 0; i < meal.count(); i++)
				{
					if (meal.get(i).getCarbs() > max)
					{
						max = meal.get(i).getCarbs();
						name = meal.get(i).getName();
					}
				}

				return name;
			}

			case TOTAL_CARBS:
			{
				final double sumCarbs = meal.getCarbs();
				final String unitGram = context.getString(R.string.common_unit_mass_gramm);
				return String.format(Locale.US, "%.0f %s", sumCarbs, unitGram);
			}

			case TOTAL_BU:
			{
				final double sumCarbs = meal.getCarbs();
				final double sumBu = sumCarbs / Utils.CARB_PER_BU;
				final String unitBU = context.getString(R.string.common_unit_mass_bu);
				return String.format(Locale.US, "%.1f %s", sumBu, unitBU);
			}

			case TOTAL_CARBS_BU:
			{
				final double sumCarbs = meal.getCarbs();
				final double sumBu = sumCarbs / Utils.CARB_PER_BU;
				final String unitGram = context.getString(R.string.common_unit_mass_gramm);
				final String unitBU = context.getString(R.string.common_unit_mass_bu);
				return String.format(Locale.US, "%.0f %s (%.1f %s)", sumCarbs, unitGram, sumBu, unitBU);
			}

			case LIST_AS_IS:
			{
				StringBuilder s = new StringBuilder();

				for (int i = 0; i < meal.count(); i++)
				{
					if (s.length() > 0)
					{
						s.append(", ");
						s.append(Utils.lowercaseFirstLetter(meal.get(i).getName()));
					}
					else
					{
						s.append(meal.get(i).getName());
					}
				}

				return s.toString();
			}

			case LIST_SORTED_BY_CARBS:
			{
				List<FoodMassed> items = new ArrayList<>();
				for (int i = 0; i < meal.count(); i++)
				{
					items.add(meal.get(i));
				}

				Collections.sort(items, new Comparator<FoodMassed>()
				{
					@Override
					public int compare(FoodMassed lhs, FoodMassed rhs)
					{
						return Double.compare(rhs.getCarbs(), lhs.getCarbs());
					}
				});

				StringBuilder s = new StringBuilder();

				for (FoodMassed item : items)
				{
					if (s.length() > 0)
					{
						s.append(", ");
						s.append(Utils.lowercaseFirstLetter(item.getName()));
					}
					else
					{
						s.append(item.getName());
					}
				}

				return s.toString();
			}

			default:
			{
				throw new IllegalArgumentException("Unknown style: " + style.name());
			}
		}
	}

	private MealFormatter()
	{
		throw new RuntimeException("Don't instantiate");
	}
}
