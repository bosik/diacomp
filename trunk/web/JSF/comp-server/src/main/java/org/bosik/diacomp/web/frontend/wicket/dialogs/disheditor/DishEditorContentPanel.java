package org.bosik.diacomp.web.frontend.wicket.dialogs.disheditor;

import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.massedpicker.FoodMassedPicker;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.FoodList;
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
		DishItem data = model.getObject().getData();
		for (int i = 0; i < data.count(); i++)
		{
			list.getContent().add(data.get(i));
		}

		// Model.of(list)

		form.add(new FoodMassedPicker("editor")
		{
			private static final long	serialVersionUID	= 6850233237789079835L;

			@Override
			public void onSelected(Versioned<FoodItem> item, Double mass)
			{
				if (item != null)
				{
					System.out.println("Selected: " + item.getData().getName());
				}
				else
				{
					System.out.println("Null selected");
				}
				System.out.println("Mass: " + mass);
			}
		});
	}
}
