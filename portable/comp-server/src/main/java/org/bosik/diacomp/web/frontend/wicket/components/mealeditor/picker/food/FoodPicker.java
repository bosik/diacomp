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
package org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.food;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormSubmitBehavior;
import org.apache.wicket.extensions.ajax.markup.html.autocomplete.AutoCompleteTextField;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.bosik.diacomp.core.entities.business.Food;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.web.backend.features.base.food.combo.FoodComboLocalService;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.bosik.merklesync.Versioned;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public abstract class FoodPicker extends Panel
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private UserInfoService userInfoService;

	@SpringBean
	private FoodComboLocalService foodService;

	private AutoCompleteTextField<String> field;

	private final IModel<String> model;

	protected FoodPicker(String id, IModel<String> model)
	{
		super(id);
		this.model = model;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		Form<Void> form = new Form<>("form");
		add(form);

		field = new AutoCompleteTextField<String>("picker", model)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected Iterator<String> getChoices(String input)
			{
				List<String> choices = new ArrayList<>();
				int userId = userInfoService.getCurrentUserId();
				List<Versioned<FoodItem>> list = foodService.findAny(userId, input);

				for (final Versioned<FoodItem> item : list)
				{
					choices.add(item.getData().getName());
				}

				return choices.iterator();
			}
		};
		field.add(new AjaxFormSubmitBehavior(form, "onchange")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				String name = field.getModelObject();
				int userId = userInfoService.getCurrentUserId();
				Versioned<FoodItem> food = foodService.findOne(userId, name);
				if (food != null)
				{
					Food item = food.getData();
					onSelected(target, Model.of(item));
				}
			}

			@Override
			protected void onError(AjaxRequestTarget target)
			{
			}
		});
		field.setOutputMarkupId(true);
		form.add(field);
	}

	public void clear()
	{
		field.setModelObject("");
	}

	public void focus(AjaxRequestTarget target)
	{
		target.focusComponent(field);
	}

	public abstract void onSelected(AjaxRequestTarget target, IModel<Food> item);
}
