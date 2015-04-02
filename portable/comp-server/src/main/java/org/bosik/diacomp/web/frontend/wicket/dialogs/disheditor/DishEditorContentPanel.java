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
package org.bosik.diacomp.web.frontend.wicket.dialogs.disheditor;

import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.editor.MealEditor;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.food.FoodList;
import org.bosik.diacomp.web.frontend.wicket.dialogs.common.CommonEditorContentPanel;
import org.bosik.merklesync.Versioned;

public abstract class DishEditorContentPanel extends CommonEditorContentPanel<DishItem>
{
	private static final long	serialVersionUID	= 1L;

	public DishEditorContentPanel(String id, final IModel<Versioned<DishItem>> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		final Versioned<DishItem> modelObject = model.getObject();

		//dish.setData(new DishItem(dish.getData()));
		//FIXME

		form.add(new TextField<String>("inputName", new PropertyModel<String>(model, "data.name")));

		FoodList list = new FoodList();
		DishItem data = modelObject.getData();
		for (int i = 0; i < data.count(); i++)
		{
			list.getContent().add(data.get(i));
		}

		form.add(new MealEditor("editor", Model.of(list)));
	}
}
