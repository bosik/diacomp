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

import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;

public class MealFormatter
{
	/**
	 * Виды сортировок продуктов в приёме пищи
	 */
	public static enum FormatStyle
	{
		/**
		 * Только первое блюдо в приёме
		 */
		FIRST,
		/**
		 * Только последнее блюдо в приёме
		 */
		LAST,
		/**
		 * Только самое углеводное блюдо
		 */
		MOST_CARBS,
		/**
		 * Все блюда в порядке убывания углеводности через запятую (не реализовано)
		 */
		LIST_CARBS; // TODO: реализовать
	}

	/**
	 * Получает текстовое представление приёма пищи в указанном режиме
	 * 
	 * @param style
	 *            Режим
	 * @return Текстовое представление
	 */
	public static String format(MealRecord meal, FormatStyle style)
	{
		switch (style)
		{
			case FIRST:
			{
				if (meal.count() == 0)
				{
					return "";
				}
				else
				{
					return meal.get(0).getName();
				}
			}
			case LAST:
			{
				if (meal.count() == 0)
				{
					return "";
				}
				else
				{
					return meal.get(meal.count() - 1).getName();
				}
			}
			case MOST_CARBS:
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
			/*
			 * case LIST_CARBS: { // TODO: реализовать }
			 */
			default:
				throw new UnsupportedOperationException("Style " + style + " is not supported yet");
		}
	}
}
