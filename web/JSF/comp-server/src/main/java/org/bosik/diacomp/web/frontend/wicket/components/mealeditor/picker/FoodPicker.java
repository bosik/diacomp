package org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormSubmitBehavior;
import org.apache.wicket.extensions.ajax.markup.html.autocomplete.AutoCompleteTextField;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.Model;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.foodbase.FoodBaseService;
import org.bosik.diacomp.web.backend.features.foodbase.service.FrontendFoodbaseService;

public abstract class FoodPicker extends Panel
{
	private static final long	serialVersionUID	= 1L;

	transient FoodBaseService	foodBase			= new FrontendFoodbaseService();

	public FoodPicker(String id)
	{
		super(id);

		Form<Void> form = new Form<Void>("form");
		add(form);

		final AutoCompleteTextField<String> field = new AutoCompleteTextField<String>("picker", new Model<String>(""))
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected Iterator<String> getChoices(String input)
			{
				List<String> choices = new ArrayList<String>();
				List<Versioned<FoodItem>> list = foodBase.findAny(input);

				for (final Versioned<FoodItem> item : list)
				{
					choices.add(item.getData().getName());
				}

				return choices.iterator();
			}
		};
		form.add(field);

		field.add(new AjaxFormSubmitBehavior(form, "onchange")
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				String name = field.getModelObject();
				Versioned<FoodItem> item = foodBase.findOne(name);
				onSelected(target, item);
			}

			@Override
			protected void onError(AjaxRequestTarget target)
			{
			}
		});
	}

	public abstract void onSelected(AjaxRequestTarget target, Versioned<FoodItem> item);
}
