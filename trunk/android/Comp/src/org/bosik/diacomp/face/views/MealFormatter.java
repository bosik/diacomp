package org.bosik.diacomp.face.views;

import org.bosik.compensation.bo.diary.records.MealRecord;

public class MealFormatter
{
	/**
	 * Виды сортировок продуктов в приёме пищи
	 * 
	 * @author Bosik
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
