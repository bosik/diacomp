package org.bosik.diacomp.web.frontend.wicket.pages.foodbase;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.RefreshingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;

public class FoodBasePage extends MasterPage
{
	private static final long	serialVersionUID	= 1L;

	public FoodBasePage(PageParameters parameters)
	{
		super(parameters);

		RefreshingView<Versioned<FoodItem>> view = new RefreshingView<Versioned<FoodItem>>("view")
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected Iterator<IModel<Versioned<FoodItem>>> getItemModels()
			{
				final List<IModel<Versioned<FoodItem>>> foodBase = new ArrayList<IModel<Versioned<FoodItem>>>();

				FoodDataProvider provider = new FoodDataProvider();
				Iterator<? extends Versioned<FoodItem>> it = provider.iterator(0, 19);
				while (it.hasNext())
				{
					foodBase.add(provider.model(it.next()));
				}

				return foodBase.iterator();
			}

			@Override
			protected void populateItem(final Item<Versioned<FoodItem>> item)
			{
				FoodItem food = item.getModelObject().getData();
				item.add(new Label("food.name", food.getName()));
				item.add(new Label("food.prots", food.getRelProts()));
				item.add(new Label("food.fats", food.getRelFats()));
				item.add(new Label("food.carbs", food.getRelCarbs()));
				item.add(new Label("food.value", food.getRelValue()));

				item.add(new AjaxEventBehavior("onclick")
				{
					private static final long	serialVersionUID	= 1L;

					@Override
					protected void onEvent(AjaxRequestTarget target)
					{
						Versioned<FoodItem> food = item.getModelObject();
						System.out.println("Food clicked: " + food.getData().getName());
					}
				});
			}
		};

		add(view);
	}
}
