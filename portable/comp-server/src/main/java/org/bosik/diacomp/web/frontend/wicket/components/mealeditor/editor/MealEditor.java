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
package org.bosik.diacomp.web.frontend.wicket.components.mealeditor.editor;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.RefreshingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.food.FoodList;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.foodMassed.inserter.FoodMassedInserter;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.foodMassed.updater.FoodMassedUpdater;

public class MealEditor extends GenericPanel<FoodList>
{
	private static final long	serialVersionUID	= 1L;

	// components
	//FoodPicker					fieldFood;
	WebMarkupContainer			container;
	boolean						readOnly;

	public MealEditor(String id, final IModel<FoodList> model, boolean readOnly)
	{
		super(id);
		setModel(model);
		this.readOnly = readOnly;
	}

	public MealEditor(String id, final IModel<FoodList> model)
	{
		this(id, model, false);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		container = new WebMarkupContainer("tableContainer");
		container.setOutputMarkupId(true);
		add(container);

		container.add(new RefreshingView<FoodMassed>("view")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(final Item<FoodMassed> item)
			{
				item.add(new FoodMassedUpdater("food", item.getModel(), item.getIndex())
				{
					private static final long serialVersionUID = 9153191446566298544L;

					@Override
					protected void onDelete(AjaxRequestTarget target, int index)
					{
						FoodList mo = getModelObject();
						List<FoodMassed> content = mo.getContent();
						content.remove(index);
						mo.setContent(content);
						setModelObject(mo);

						target.add(container);
					}
				});

				//				item.add(new AjaxEventBehavior("onclick")
				//				{
				//					private static final long	serialVersionUID	= 1L;
				//
				//					@Override
				//					protected void onEvent(AjaxRequestTarget target)
				//					{
				//						Versioned<FoodItem> food = item.getModelObject();
				//						System.out.println("Opening: " + food.getData().getName());
				//
				//						foodEditor.show(target, Model.of(food));
				//					}
				//				});
			}

			@Override
			protected Iterator<IModel<FoodMassed>> getItemModels()
			{
				List<IModel<FoodMassed>> list = new ArrayList<IModel<FoodMassed>>();
				for (FoodMassed item : getModelObject().getContent())
				{
					list.add(Model.of(item));
				}

				return list.iterator();
			}
		});

		final FoodMassedInserter foodMassedInserter = new FoodMassedInserter("picker")
		{
			private static final long serialVersionUID = 6850233237789079835L;

			@Override
			public void onSelected(AjaxRequestTarget target, IModel<FoodMassed> item)
			{
				if (item.getObject().getName() != null)
				{
					FoodList modelObject = getModelObject();
					modelObject.getContent().add(item.getObject());
					setModelObject(modelObject);

					target.add(container);
				}
				else
				{
					System.out.println("Null selected");
				}
			}
		};
		if (readOnly)
		{
			foodMassedInserter.setOutputMarkupId(true);
			foodMassedInserter.setVisible(false);
		}
		add(foodMassedInserter);
	}
}
