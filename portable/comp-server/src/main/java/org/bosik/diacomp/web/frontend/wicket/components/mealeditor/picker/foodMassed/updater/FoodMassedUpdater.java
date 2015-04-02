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
package org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.foodMassed.updater;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.bosik.diacomp.core.entities.business.Food;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.foodMassed.common.FoodMassedPicker;

public class FoodMassedUpdater extends Panel
{
	private static final long	serialVersionUID	= 1L;
	private IModel<FoodMassed>	model;
	private int					index;

	public FoodMassedUpdater(String id, IModel<FoodMassed> model, int index)
	{
		super(id, model);
		this.model = model;
		this.index = index;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		//		Image icon = new Image("icon", "");
		//		add(icon);

		FoodMassedPicker picker = new FoodMassedPicker("picker", model)
		{
			@Override
			public void onFoodChanged(AjaxRequestTarget target, IModel<Food> food)
			{
				target.focusComponent(fieldMass);
				target.add(fieldMass);
				FoodMassedUpdater.this.onFoodChanged(target, food);
			}

			@Override
			public void onMassChanged(AjaxRequestTarget target, IModel<Double> mass)
			{
				//		target.focusComponent(fieldFood);
				//		target.add(fieldMass, fieldFood);
				FoodMassedUpdater.this.onMassChanged(target, mass);
			}
		};
		add(picker);

		AjaxFallbackLink<Void> linkDelete = new AjaxFallbackLink<Void>("delete")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				FoodMassedUpdater.this.onDelete(target, index);
			}
		};
		add(linkDelete);
	}

	protected void onFoodChanged(AjaxRequestTarget target, IModel<Food> food)
	{
	};

	protected void onMassChanged(AjaxRequestTarget target, IModel<Double> mass)
	{
	};

	protected void onDelete(AjaxRequestTarget target, int index)
	{
	};
}
