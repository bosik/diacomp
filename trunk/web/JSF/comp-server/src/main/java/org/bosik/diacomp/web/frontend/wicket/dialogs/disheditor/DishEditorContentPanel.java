package org.bosik.diacomp.web.frontend.wicket.dialogs.disheditor;

import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.editor.MealEditor;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.simple.FoodList;
import org.bosik.diacomp.web.frontend.wicket.dialogs.common.CommonEditorContentPanel;

public abstract class DishEditorContentPanel extends CommonEditorContentPanel<DishItem>
{
	private static final long	serialVersionUID	= 1L;

	public DishEditorContentPanel(String id, final Model<Versioned<DishItem>> model)
	{
		super(id, model);

		final Versioned<DishItem> modelObject = model.getObject();

		entity = new Versioned<DishItem>(modelObject);
		//dish.setData(new DishItem(dish.getData()));
		//FIXME

		form.add(new TextField<String>("inputName", new PropertyModel<String>(entity, "data.name")));

		FoodList list = new FoodList();
		DishItem data = modelObject.getData();
		for (int i = 0; i < data.count(); i++)
		{
			list.getContent().add(data.get(i));
		}

		form.add(new MealEditor("editor", Model.of(list)));
	}
}
