package org.bosik.diacomp.web.frontend.wicket.dialogs.foodeditor;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxFallbackButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;

public abstract class FoodEditorContentPanel extends Panel
{
	private static final long	serialVersionUID	= 1L;

	Versioned<FoodItem>			food;

	public FoodEditorContentPanel(String id, final Model<Versioned<FoodItem>> model)
	{
		super(id);

		// clone
		food = new Versioned<FoodItem>(model.getObject());
		food.setData(new FoodItem(food.getData()));

		Form form = new Form("form");
		add(form);

		//		add(new AjaxLink("selectionLink")
		//		{
		//			private static final long	serialVersionUID	= 2551387346578773401L;
		//
		//			@Override
		//			public void onClick(AjaxRequestTarget target)
		//			{
		//				onSelect(target, new String("Selection using the link."));
		//			}
		//		});

		form.add(new TextField<String>("inputName", new PropertyModel<String>(food, "data.name")));

		form.add(new AjaxFallbackButton("buttonSelect", form)
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target, Form form)
			{
				onSelect(target, Model.of(food));
			}
		});

		// Add a cancel / close button.
		form.add(new AjaxFallbackButton("buttonClose", form)
		{
			private static final long	serialVersionUID	= -3966833383602736092L;

			@Override
			public void onSubmit(AjaxRequestTarget target, Form form)
			{
				onCancel(target);
			}
		});
	}

	abstract void onCancel(AjaxRequestTarget target);

	abstract void onSelect(AjaxRequestTarget target, Model<Versioned<FoodItem>> model);
}
