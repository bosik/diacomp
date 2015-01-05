package org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.foodMassed.common;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxCallListener;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.image.Image;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.bosik.diacomp.core.entities.business.Food;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.food.FoodPicker;

public abstract class FoodMassedPicker extends Panel
{
	private static final long		serialVersionUID	= 1L;

	protected IModel<FoodMassed>	model;

	// values
	//		Food						selectedItem;
	IModel<Double>					mass;

	// components
	protected Image					icon;
	protected FoodPicker			fieldFood;
	protected TextField<Double>		fieldMass;

	public FoodMassedPicker(String id, IModel<FoodMassed> model)
	{
		super(id);
		this.model = model;
		mass = new PropertyModel<Double>(model, "mass");
	}

	public FoodMassedPicker(String id)
	{
		this(id, Model.of(new FoodMassed()));
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		icon = new Image("icon", "");
		add(icon);

		fieldFood = new FoodPicker("picker", Model.of(model.getObject().getName()))
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			public void onSelected(AjaxRequestTarget target, IModel<Food> food)
			{
				// copy food info to model

				Food newFood = food.getObject();

				FoodMassed modelObject = model.getObject();
				modelObject.setName(newFood.getName());
				modelObject.setRelProts(newFood.getRelProts());
				modelObject.setRelFats(newFood.getRelFats());
				modelObject.setRelCarbs(newFood.getRelCarbs());
				modelObject.setRelValue(newFood.getRelValue());
				model.setObject(modelObject);

				onFoodChanged(target, food);
			}
		};
		fieldFood.setOutputMarkupId(true);
		add(fieldFood);

		fieldMass = new TextField<Double>("mass", mass);
		fieldMass.add(new AjaxFormComponentUpdatingBehavior("keydown")
		{
			private static final long	serialVersionUID	= 1072515919159765189L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
			}
		});
		fieldMass.add(new AjaxFormComponentUpdatingBehavior("onblur")
		{
			private static final long	serialVersionUID	= 1072515919159765189L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
			}
		});
		fieldMass.add(new AjaxEventBehavior("keydown")
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
			{
				super.updateAjaxAttributes(attributes);

				attributes.getAjaxCallListeners().add(new AjaxCallListener()
				{
					private static final long	serialVersionUID	= -846467011322058537L;

					@Override
					public CharSequence getPrecondition(Component component)
					{
						return "return (Wicket.Event.keyCode(attrs.event) === 13);";
					}
				});
				attributes.setAllowDefault(true);
			}

			@Override
			protected void onEvent(AjaxRequestTarget target)
			{
				if (mass == null)
				{
					//					// TODO: is it possible?
					//					fieldFood.focus(target);
				}
				else
				{
					// copy the mass to model

					FoodMassed modelObject = model.getObject();
					modelObject.setMass(mass.getObject());
					model.setObject(modelObject);

					onMassChanged(target, mass);
				}
			}
		});
		add(fieldMass);
	}

	public abstract void onFoodChanged(AjaxRequestTarget target, IModel<Food> food);

	public abstract void onMassChanged(AjaxRequestTarget target, IModel<Double> mass);
}
