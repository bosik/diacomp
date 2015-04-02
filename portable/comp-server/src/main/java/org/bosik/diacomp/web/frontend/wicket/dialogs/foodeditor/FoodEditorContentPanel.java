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
package org.bosik.diacomp.web.frontend.wicket.dialogs.foodeditor;

import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.web.frontend.wicket.dialogs.common.CommonEditorContentPanel;
import org.bosik.merklesync.Versioned;

public abstract class FoodEditorContentPanel extends CommonEditorContentPanel<FoodItem>
{
	private static final long	serialVersionUID	= 1L;

	public FoodEditorContentPanel(String id, IModel<Versioned<FoodItem>> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		//final Versioned<FoodItem> modelObject = model.getObject();

		//		food = new Versioned<FoodItem>(modelObject);
		//		food.setData(new FoodItem(food.getData()));

		form.add(new TextField<String>("inputName", new PropertyModel<String>(model, "data.name")));
		form.add(new TextField<Double>("inputProts", new PropertyModel<Double>(model, "data.relProts")));
		form.add(new TextField<Double>("inputFats", new PropertyModel<Double>(model, "data.relFats")));
		form.add(new TextField<Double>("inputCarbs", new PropertyModel<Double>(model, "data.relCarbs")));
		form.add(new TextField<Double>("inputValue", new PropertyModel<Double>(model, "data.relValue")));
	}
}
