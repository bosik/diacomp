package org.bosik.diacomp.web.frontend.wicket.dialogs.foodeditor;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxFallbackButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;

public abstract class FoodEditorContentPanel extends Panel
{
	private static final long	serialVersionUID	= 1L;

	Versioned<FoodItem>			food;
	IModel<Versioned<FoodItem>>	model;

	public FoodEditorContentPanel(String id, IModel<Versioned<FoodItem>> model)
	{
		super(id);
		this.model = model;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		final Versioned<FoodItem> modelObject = model.getObject();

		food = new Versioned<FoodItem>(modelObject);
		food.setData(new FoodItem(food.getData()));

		Form<Void> form = new Form<Void>("form");
		add(form);

		form.add(new TextField<String>("inputName", new PropertyModel<String>(food, "data.name")));
		form.add(new TextField<Double>("inputProts", new PropertyModel<Double>(food, "data.relProts")));
		form.add(new TextField<Double>("inputFats", new PropertyModel<Double>(food, "data.relFats")));
		form.add(new TextField<Double>("inputCarbs", new PropertyModel<Double>(food, "data.relCarbs")));
		form.add(new TextField<Double>("inputValue", new PropertyModel<Double>(food, "data.relValue")));

		form.add(new AjaxFallbackButton("buttonSave", form)
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target, Form<?> form)
			{
				onSave(target, Model.of(food));
			}
		});

		form.add(new AjaxFallbackButton("buttonCancel", form)
		{
			private static final long	serialVersionUID	= -3966833383602736092L;

			@Override
			public void onSubmit(AjaxRequestTarget target, Form<?> form)
			{
				onCancel(target);
			}
		});
	}

	abstract void onCancel(AjaxRequestTarget target);

	abstract void onSave(AjaxRequestTarget target, IModel<Versioned<FoodItem>> model);
}
