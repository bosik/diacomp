package org.bosik.diacomp.web.frontend.wicket.components.mealeditor.editor;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.RefreshingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.bosik.diacomp.core.entities.business.Food;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.massed.FoodMassedPicker;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.simple.FoodList;

public class MealEditor extends Panel
{
	private static final long	serialVersionUID	= 1L;

	// components
	//FoodPicker					fieldFood;
	WebMarkupContainer			container;

	public MealEditor(String id, final IModel<FoodList> model)
	{
		super(id);

		container = new WebMarkupContainer("tableContainer");
		container.setOutputMarkupId(true);
		add(container);

		container.add(new RefreshingView<FoodMassed>("view")
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected void populateItem(final Item<FoodMassed> item)
			{
				FoodMassed food = item.getModelObject();

				item.add(new Label("food.name", food.getName()));
				item.add(new Label("food.mass", food.getMass()));

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
				for (FoodMassed item : model.getObject().getContent())
				{
					list.add(Model.of(item));
				}

				return list.iterator();
			}
		});

		add(new FoodMassedPicker("picker")
		{
			private static final long	serialVersionUID	= 6850233237789079835L;

			@Override
			public void onSelected(AjaxRequestTarget target, Food item, Double mass)
			{
				
				if (item != null)
				{
					model.getObject().getContent().add(new FoodMassed(item, mass));
					target.add(container);
				}
				else
				{
					System.out.println("Null selected");
				}
			}
		});
	}
}
