package org.bosik.diacomp.web.frontend.wicket.pages.foodbase;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
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

		final List<IModel<Versioned<FoodItem>>> foodBase = new ArrayList<IModel<Versioned<FoodItem>>>();

		// populate list of contacts to be displayed

		FoodDataProvider dp = new FoodDataProvider();
		Iterator<? extends Versioned<FoodItem>> it = dp.iterator(0, 9);
		while (it.hasNext())
		{
			foodBase.add(dp.model(it.next()));
		}

		// create the refreshing view
		RefreshingView<Versioned<FoodItem>> view = new RefreshingView<Versioned<FoodItem>>("view")
		{
			private static final long	serialVersionUID	= 1L;

			/**
			 * Return an iterator over models for items in the view
			 */
			@Override
			protected Iterator<IModel<Versioned<FoodItem>>> getItemModels()
			{
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
			}
		};

		add(view);
	}
}
