package org.bosik.diacomp.web.frontend.wicket.components.mealeditor.massedpicker;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.PropertyModel;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.FoodPicker;

public abstract class FoodMassedPicker extends Panel
{
	private static final long	serialVersionUID	= 1L;

	// values
	double						mass;
	Versioned<FoodItem>			selectedItem;

	// components
	TextField<Double>			fieldMass;
	FoodPicker					picker;

	public FoodMassedPicker(String id)
	{
		super(id);

		picker = new FoodPicker("picker")
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			public void onSelected(AjaxRequestTarget target, Versioned<FoodItem> item)
			{
				selectedItem = item;
				target.focusComponent(fieldMass);
			}
		};
		add(picker);

		fieldMass = new TextField<Double>("mass", new PropertyModel<Double>(this, "mass"));
		fieldMass.add(new AjaxFormComponentUpdatingBehavior("keyup")
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				onSelected(selectedItem, mass);
			}
		});
		add(fieldMass);
	}

	public abstract void onSelected(Versioned<FoodItem> item, Double mass);
}
