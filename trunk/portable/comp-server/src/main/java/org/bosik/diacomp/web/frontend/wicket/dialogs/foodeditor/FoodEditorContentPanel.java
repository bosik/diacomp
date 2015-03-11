package org.bosik.diacomp.web.frontend.wicket.dialogs.foodeditor;

import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.web.frontend.wicket.dialogs.common.CommonEditorContentPanel;

public abstract class FoodEditorContentPanel extends CommonEditorContentPanel<FoodItem>
{
	private static final long	serialVersionUID	= 1L;

	public FoodEditorContentPanel(String id, IModel<Versioned<FoodItem>> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		//final Versioned<FoodItem> modelObject = model.getObject();

		//		food = new Versioned<FoodItem>(modelObject);
		//		food.setData(new FoodItem(food.getData()));

		form.add(new TextField<String>("inputName", new PropertyModel<String>(model, "data.name")));
		form.add(new TextField<Double>("inputProts", new PropertyModel<Double>(model, "data.relProts")));
		form.add(new TextField<Double>("inputFats", new PropertyModel<Double>(model, "data.relFats")));
		form.add(new TextField<Double>("inputCarbs", new PropertyModel<Double>(model, "data.relCarbs")));
		form.add(new TextField<Double>("inputValue", new PropertyModel<Double>(model, "data.relValue")));
	}
}
