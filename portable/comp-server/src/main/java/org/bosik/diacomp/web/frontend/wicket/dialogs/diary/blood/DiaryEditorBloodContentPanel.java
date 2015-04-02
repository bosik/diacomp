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
package org.bosik.diacomp.web.frontend.wicket.dialogs.diary.blood;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.apache.wicket.extensions.yui.calendar.DateTimeField;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.web.frontend.wicket.dialogs.common.CommonEditorContentPanel;
import org.bosik.merklesync.Versioned;

public abstract class DiaryEditorBloodContentPanel extends CommonEditorContentPanel<BloodRecord>
{
	private static final long	serialVersionUID	= 1L;

	public DiaryEditorBloodContentPanel(String id, IModel<Versioned<BloodRecord>> model)
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

		//Date and time fields
		DateTimeField dateTimeField = new DateTimeField("dateTimeField", new PropertyModel<Date>(model, "data.time"));
		form.add(dateTimeField);

		form.add(new TextField<String>("inputValue", new PropertyModel<String>(model, "data.value")));

		List<Integer> choices = new ArrayList<Integer>();
		for (int i = 0; i < 10; i++)
		{
			choices.add(i);
		}

		DropDownChoice<Integer> ddc = new DropDownChoice<Integer>("inputFinger", new PropertyModel<Integer>(model,
				"data.finger"), choices, new IChoiceRenderer<Integer>()
		{
			private static final long	serialVersionUID	= 1478780792803547209L;

			@Override
			public Object getDisplayValue(Integer object)
			{
				return getString("finger.long." + object);
			}

			@Override
			public String getIdValue(Integer object, int index)
			{
				return object.toString();
			}
		});

		form.add(ddc);
	}
}
