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
package org.bosik.diacomp.web.frontend.wicket.components.diary.meal;

import java.util.ArrayList;
import java.util.List;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.editor.MealEditor;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.food.FoodList;
import org.bosik.merklesync.Versioned;

public class DiaryPanelMeal extends Panel
{
	private static final long		serialVersionUID	= 1L;

	IModel<Versioned<MealRecord>>	model;

	public DiaryPanelMeal(String id, Model<Versioned<MealRecord>> model)
	{
		super(id);
		this.model = model;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		add(new Label("time", Utils.formatTimeLocalShort(model.getObject().getData().getTime())));
		add(new MealEditor("content", new IModel<FoodList>()
		{
			private static final long	serialVersionUID	= 7161066379939942868L;

			@Override
			public void detach()
			{
			}

			@Override
			public void setObject(FoodList object)
			{
				Versioned<MealRecord> record = model.getObject();
				record.getData().clear();

				for (FoodMassed item : object.getContent())
				{
					record.getData().add(item);
				}

				model.setObject(record);
			}

			@Override
			public FoodList getObject()
			{
				Versioned<MealRecord> data = model.getObject();

				List<FoodMassed> content = new ArrayList<FoodMassed>();
				for (int i = 0; i < data.getData().count(); i++)
				{
					content.add(data.getData().get(i));
				}

				FoodList list = new FoodList();
				list.setContent(content);

				return list;
			}
		}));
	}
}
