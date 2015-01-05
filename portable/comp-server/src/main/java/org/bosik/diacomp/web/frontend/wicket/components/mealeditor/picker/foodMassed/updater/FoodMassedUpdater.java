package org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.foodMassed.updater;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.bosik.diacomp.core.entities.business.Food;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.foodMassed.common.FoodMassedPicker;

public class FoodMassedUpdater extends Panel
{
	private static final long	serialVersionUID	= 1L;
	private IModel<FoodMassed>	model;
	private int					index;

	public FoodMassedUpdater(String id, IModel<FoodMassed> model, int index)
	{
		super(id, model);
		this.model = model;
		this.index = index;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		//		Image icon = new Image("icon", "");
		//		add(icon);

		FoodMassedPicker picker = new FoodMassedPicker("picker", model)
		{
			@Override
			public void onFoodChanged(AjaxRequestTarget target, IModel<Food> food)
			{
				target.focusComponent(fieldMass);
				target.add(fieldMass);
				FoodMassedUpdater.this.onFoodChanged(target, food);
			}

			@Override
			public void onMassChanged(AjaxRequestTarget target, IModel<Double> mass)
			{
				//		target.focusComponent(fieldFood);
				//		target.add(fieldMass, fieldFood);
				FoodMassedUpdater.this.onMassChanged(target, mass);
			}
		};
		add(picker);

		AjaxFallbackLink<Void> linkDelete = new AjaxFallbackLink<Void>("delete")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				FoodMassedUpdater.this.onDelete(target, index);
			}
		};
		add(linkDelete);
	}

	protected void onFoodChanged(AjaxRequestTarget target, IModel<Food> food)
	{
	};

	protected void onMassChanged(AjaxRequestTarget target, IModel<Double> mass)
	{
	};

	protected void onDelete(AjaxRequestTarget target, int index)
	{
	};
}
