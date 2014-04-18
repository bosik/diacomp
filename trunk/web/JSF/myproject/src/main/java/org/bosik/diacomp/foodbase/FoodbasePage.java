package org.bosik.diacomp.foodbase;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.data.DataView;
import org.apache.wicket.model.AbstractReadOnlyModel;

public class FoodbasePage extends WebPage
{
	private static final long	serialVersionUID	= 1L;

	public FoodbasePage()
	{
		add(new DataView<Food>("simple", new FoodDataProvider())
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected void populateItem(final Item<Food> item)
			{
				Food food = item.getModelObject();
				item.add(new Label("contactid", String.valueOf(food.getId())));
				item.add(new Label("firstname", food.getName()));

				item.add(AttributeModifier.replace("class", new AbstractReadOnlyModel<String>()
				{
					private static final long	serialVersionUID	= 1L;

					@Override
					public String getObject()
					{
						return (item.getIndex() % 2 == 1) ? "even" : "odd";
					}
				}));
			}
		});
	}
}
