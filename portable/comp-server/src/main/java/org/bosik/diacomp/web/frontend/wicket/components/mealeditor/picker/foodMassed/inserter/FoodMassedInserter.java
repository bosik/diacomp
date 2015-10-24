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
package org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.foodMassed.inserter;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.bosik.diacomp.core.entities.business.Food;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.foodMassed.common.FoodMassedPicker;

public abstract class FoodMassedInserter extends Panel
{
	private static final long serialVersionUID = 1L;

	public FoodMassedInserter(String id)
	{
		super(id);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		FoodMassedPicker picker = new FoodMassedPicker("picker")
		{
			private static final long serialVersionUID = -5896695927537265374L;

			@Override
			public void onFoodChanged(AjaxRequestTarget target, IModel<Food> food)
			{
				target.focusComponent(fieldMass);
				target.add(fieldMass);
			}

			@Override
			public void onMassChanged(AjaxRequestTarget target, IModel<Double> mass)
			{
				// call the event

				onSelected(target, model);

				// proceed the focus and stuff

				model.setObject(new FoodMassed());

				fieldFood.clear();
				fieldMass.clearInput();
				fieldFood.focus(target);
				target.add(fieldFood, fieldMass);
			}
		};
		add(picker);
	}

	public abstract void onSelected(AjaxRequestTarget target, IModel<FoodMassed> item);
}
