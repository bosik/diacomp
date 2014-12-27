package org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.foodMassed.inserter;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.model.IModel;
import org.bosik.diacomp.core.entities.business.Food;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.foodMassed.common.FoodMassedPicker;

public abstract class FoodMassedInserter extends FoodMassedPicker
{
	private static final long	serialVersionUID	= 1L;

	public FoodMassedInserter(String id)
	{
		super(id);
	}

	@Override
	public void onFoodChanged(AjaxRequestTarget target, IModel<Food> item)
	{
		// copy food info to model

		Food newFood = item.getObject();

		FoodMassed modelObject = model.getObject();
		modelObject.setName(newFood.getName());
		modelObject.setRelProts(newFood.getRelProts());
		modelObject.setRelFats(newFood.getRelFats());
		modelObject.setRelCarbs(newFood.getRelCarbs());
		modelObject.setRelValue(newFood.getRelValue());
		model.setObject(modelObject);

		// proceed the focus

		target.focusComponent(fieldMass);
	}

	@Override
	public void onMassChanged(AjaxRequestTarget target, IModel<Double> mass)
	{
		// copy the mass to model

		FoodMassed modelObject = model.getObject();
		modelObject.setMass(mass.getObject());
		model.setObject(modelObject);

		// call the event

		onSelected(target, model);

		// proceed the focus and stuff

		model.setObject(new FoodMassed());

		fieldFood.clear();
		fieldMass.clearInput();
		fieldFood.focus(target);
		target.add(fieldFood, fieldMass);
	}

	public abstract void onSelected(AjaxRequestTarget target, IModel<FoodMassed> item);
}
