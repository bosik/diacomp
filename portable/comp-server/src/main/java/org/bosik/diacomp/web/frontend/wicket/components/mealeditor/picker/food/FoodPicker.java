package org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.food;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormSubmitBehavior;
import org.apache.wicket.extensions.ajax.markup.html.autocomplete.AutoCompleteTextField;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.bosik.diacomp.core.entities.business.Food;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.foodbase.FoodBaseService;

public abstract class FoodPicker extends Panel
{
	private static final long				serialVersionUID	= 1L;

	@SpringBean
	private FoodBaseService					foodBase;

	private AutoCompleteTextField<String>	field;

	private final IModel<String>			model;

	public FoodPicker(String id, IModel<String> model)
	{
		super(id);
		this.model = model;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		Form<Void> form = new Form<Void>("form");
		add(form);

		field = new AutoCompleteTextField<String>("picker", model)
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
		field.add(new AjaxFormSubmitBehavior(form, "onchange")
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				String name = field.getModelObject();
				Versioned<FoodItem> food = foodBase.findOne(name);
				if (food != null)
				{
					Food item = food.getData();
					onSelected(target, Model.of(item));
				}
			}

			@Override
			protected void onError(AjaxRequestTarget target)
			{
			}
		});
		field.setOutputMarkupId(true);
		form.add(field);
	}

	public void clear()
	{
		field.setModelObject("");
	}

	public void focus(AjaxRequestTarget target)
	{
		target.focusComponent(field);
	}

	public abstract void onSelected(AjaxRequestTarget target, IModel<Food> item);
}
