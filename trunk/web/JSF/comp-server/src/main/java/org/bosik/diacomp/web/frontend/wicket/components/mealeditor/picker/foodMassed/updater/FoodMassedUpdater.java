package org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.foodMassed.updater;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.model.IModel;
import org.bosik.diacomp.core.entities.business.Food;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.foodMassed.common.FoodMassedPicker;

public class FoodMassedUpdater extends FoodMassedPicker
{
	private static final long	serialVersionUID	= 1L;

	public FoodMassedUpdater(String id, IModel<FoodMassed> model)
	{
		super(id, model);
		// TODO Auto-generated constructor stub
	}

	@Override
	public void onFoodChanged(AjaxRequestTarget target, IModel<Food> food)
	{
		// TODO Auto-generated method stub

	}

	@Override
	public void onMassChanged(AjaxRequestTarget target, IModel<Double> mass)
	{
		// TODO Auto-generated method stub

	}

}
