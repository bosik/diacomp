package org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.foodMassed.inserter;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.bosik.diacomp.core.entities.business.Food;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.foodMassed.common.FoodMassedPicker;

public abstract class FoodMassedInserter extends Panel
{
	private static final long	serialVersionUID	= 1L;

	public FoodMassedInserter(String id)
	{
		super(id);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		FoodMassedPicker picker = new FoodMassedPicker("picker")
		{
			@Override
			public void onFoodChanged(AjaxRequestTarget target, IModel<Food> food)
			{
				target.focusComponent(fieldMass);
				target.add(fieldMass);
			}

			@Override
			public void onMassChanged(AjaxRequestTarget target, IModel<Double> mass)
			{
				// call the event

				onSelected(target, model);

				// proceed the focus and stuff

				model.setObject(new FoodMassed());

				fieldFood.clear();
				fieldMass.clearInput();
				fieldFood.focus(target);
				target.add(fieldFood, fieldMass);
			}
		};
		add(picker);
	}

	public abstract void onSelected(AjaxRequestTarget target, IModel<FoodMassed> item);
}
