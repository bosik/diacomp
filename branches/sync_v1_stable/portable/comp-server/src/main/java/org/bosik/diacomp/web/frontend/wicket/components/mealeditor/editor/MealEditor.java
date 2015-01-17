package org.bosik.diacomp.web.frontend.wicket.components.mealeditor.editor;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.RefreshingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.food.FoodList;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.foodMassed.inserter.FoodMassedInserter;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.foodMassed.updater.FoodMassedUpdater;

public class MealEditor extends Panel
{
	private static final long	serialVersionUID	= 1L;

	// components
	//FoodPicker					fieldFood;
	WebMarkupContainer			container;
	IModel<FoodList>			model;

	public MealEditor(String id, final IModel<FoodList> model)
	{
		super(id);
		this.model = model;
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
			private static final long	serialVersionUID	= 1L;

			@Override
			protected void populateItem(final Item<FoodMassed> item)
			{
				item.add(new FoodMassedUpdater("food", item.getModel()));

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

		add(new FoodMassedInserter("picker")
		{
			private static final long	serialVersionUID	= 6850233237789079835L;

			@Override
			public void onSelected(AjaxRequestTarget target, IModel<FoodMassed> item)
			{
				if (item != null)
				{
					FoodList modelObject = MealEditor.this.model.getObject();
					modelObject.getContent().add(item.getObject());
					MealEditor.this.model.setObject(modelObject);

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
