package org.bosik.diacomp.web.frontend.wicket.components.mealeditor.massedpicker;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxCallListener;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
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
	Versioned<FoodItem>			selectedItem;
	Double						mass;

	// components
	FoodPicker					fieldFood;
	TextField<Double>			fieldMass;

	public FoodMassedPicker(String id)
	{
		super(id);

		fieldFood = new FoodPicker("picker")
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			public void onSelected(AjaxRequestTarget target, Versioned<FoodItem> item)
			{
				selectedItem = item;
				target.focusComponent(fieldMass);
			}
		};
		fieldFood.setOutputMarkupId(true);
		add(fieldFood);

		fieldMass = new TextField<Double>("mass", new PropertyModel<Double>(this, "mass"));
		fieldMass.add(new AjaxFormComponentUpdatingBehavior("keydown")
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
				onSelected(selectedItem, mass);

				fieldFood.clear();
				fieldMass.setModelObject(null);
				fieldFood.focus(target);
				target.add(fieldFood, fieldMass);
			}
		});
		add(fieldMass);
	}

	public abstract void onSelected(Versioned<FoodItem> item, Double mass);
}
