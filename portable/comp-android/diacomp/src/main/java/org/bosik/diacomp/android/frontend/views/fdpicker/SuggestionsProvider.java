/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2025 Nikita Bosik
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
package org.bosik.diacomp.android.frontend.views.fdpicker;

import android.content.Context;
import org.bosik.diacomp.android.backend.features.dishbase.DishBaseLocalService;
import org.bosik.diacomp.android.backend.features.foodbase.FoodBaseLocalService;
import org.bosik.diacomp.core.entities.business.interfaces.Named;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.android.backend.features.search.UsageIndexDiary;
import org.bosik.diacomp.android.backend.features.search.UsageIndexDishbase;
import org.bosik.merklesync.Versioned;

import java.util.ArrayList;
import java.util.List;

public class SuggestionsProvider
{
	public enum Mode
	{
		MEAL_EDITOR,
		DISH_EDITOR
	}

	public static List<Versioned<? extends Named>> getSuggestions(Context context, Mode mode)
	{
		// prepare sources
		FoodBaseService foodBase = FoodBaseLocalService.getInstance(context);
		DishBaseService dishBase = DishBaseLocalService.getInstance(context);

		// build lists
		List<Versioned<? extends Named>> data = new ArrayList<>();
		data.addAll(foodBase.findAll(false));
		data.addAll(dishBase.findAll(false));

		// sort
		switch (mode)
		{
			case MEAL_EDITOR:
				UsageIndexDiary.sort(data);
				break;

			case DISH_EDITOR:
				UsageIndexDishbase.sort(data);
				break;

			default:
				throw new UnsupportedOperationException("Mode not supported: " + mode);
		}

		return data;
	}
}
