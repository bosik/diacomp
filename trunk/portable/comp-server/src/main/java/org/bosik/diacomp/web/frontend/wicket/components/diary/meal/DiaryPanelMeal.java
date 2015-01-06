package org.bosik.diacomp.web.frontend.wicket.components.diary.meal;

import java.util.ArrayList;
import java.util.List;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.editor.MealEditor;
import org.bosik.diacomp.web.frontend.wicket.components.mealeditor.picker.food.FoodList;

public class DiaryPanelMeal extends Panel
{
	private static final long	serialVersionUID	= 1L;

	private IModel<MealRecord>	model;

	public DiaryPanelMeal(String id, IModel<MealRecord> model)
	{
		super(id);
		this.model = model;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		add(new Label("time", Utils.formatTimeLocalShort(model.getObject().getTime())));
		add(new MealEditor("content", new IModel<FoodList>()
		{
			@Override
			public void detach()
			{
			}

			@Override
			public void setObject(FoodList object)
			{
				MealRecord record = model.getObject();
				record.clear();

				for (FoodMassed item : object.getContent())
				{
					record.add(item);
				}

				model.setObject(record);
			}

			@Override
			public FoodList getObject()
			{
				MealRecord data = model.getObject();

				List<FoodMassed> content = new ArrayList<FoodMassed>();
				for (int i = 0; i < data.count(); i++)
				{
					content.add(data.get(i));
				}

				FoodList list = new FoodList();
				list.setContent(content);

				return list;
			}
		}));
	}
}
