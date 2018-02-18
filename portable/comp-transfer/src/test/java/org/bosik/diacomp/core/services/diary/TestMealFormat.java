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
package org.bosik.diacomp.core.services.diary;

import org.bosik.diacomp.core.utils.AbstractTestCoded;

import java.util.HashMap;

public class TestMealFormat extends AbstractTestCoded<MealFormat>
{
	@Override
	protected Class<MealFormat> getEntityClass()
	{
		return MealFormat.class;
	}

	@Override
	protected HashMap<MealFormat, String> getPublishedCodes()
	{
		return new HashMap<MealFormat, String>()
		{
			{
				add("9daabf78ed994e268f2e8c905564c82d", MealFormat.SINGLE_MOST_CARBS);
				add("7a0c32c15c6e4ac6bf2b2b0d350810df", MealFormat.TOTAL_CARBS);
				add("d3e249c5fb7f4d40872d4a1538be495f", MealFormat.TOTAL_BU);
				add("076499f9c70740be8dd8f227e579bc9f", MealFormat.TOTAL_CARBS_BU);
				add("2bae9dbcb7f9421ead21eaf576a03229", MealFormat.LIST_AS_IS);
				add("f2dbaeeafffb487496d953c619e9479e", MealFormat.LIST_SORTED_BY_CARBS);
			}

			private void add(String code, MealFormat entry)
			{
				put(entry, code);
			}
		};
	}
}
