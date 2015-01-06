package org.bosik.diacomp.web.frontend.wicket.components.diary.day;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.RefreshingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.frontend.wicket.components.diary.blood.DiaryPanelBlood;
import org.bosik.diacomp.web.frontend.wicket.components.diary.ins.DiaryPanelIns;
import org.bosik.diacomp.web.frontend.wicket.components.diary.meal.DiaryPanelMeal;
import org.bosik.diacomp.web.frontend.wicket.components.diary.note.DiaryPanelNote;

public class DiaryPanelDay extends Panel
{
	private static final long			serialVersionUID	= 1L;

	IModel<DiaryPanelDayModelObject>	model;

	public DiaryPanelDay(String id, IModel<DiaryPanelDayModelObject> model)
	{
		super(id);
		this.model = model;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		final DiaryPanelDayModelObject data = model.getObject();

		add(new Label("caption", Utils.formatDateLocal(data.getDate())));
		add(new RefreshingView<Versioned<DiaryRecord>>("diaryRecord")
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected Iterator<IModel<Versioned<DiaryRecord>>> getItemModels()
			{
				final List<IModel<Versioned<DiaryRecord>>> list = new ArrayList<IModel<Versioned<DiaryRecord>>>();

				//				FoodDataProvider provider = new FoodDataProvider();
				//				Iterator<? extends Versioned<FoodItem>> it = provider.iterator(0, 19);
				//				while (it.hasNext())
				//				{
				//					foodBase.add(provider.model(it.next()));
				//				}

				for (Versioned<DiaryRecord> item : data.getItems())
				{
					DiaryRecord data = item.getData();
					if ((data instanceof BloodRecord) || (data instanceof InsRecord) || (data instanceof MealRecord)
							|| (data instanceof NoteRecord))
					{
						list.add(Model.of(item));
					}
				}

				return list.iterator();
			}

			@Override
			protected void populateItem(final Item<Versioned<DiaryRecord>> item)
			{
				DiaryRecord record = item.getModelObject().getData();

				if (record instanceof BloodRecord)
				{
					item.add(new DiaryPanelBlood("diaryRecordPanel", Model.of((BloodRecord)record)));
				}
				else if (record instanceof InsRecord)
				{
					item.add(new DiaryPanelIns("diaryRecordPanel", Model.of((InsRecord)record)));
				}
				else if (record instanceof MealRecord)
				{
					item.add(new DiaryPanelMeal("diaryRecordPanel", Model.of((MealRecord)record)));
				}
				else if (record instanceof NoteRecord)
				{
					item.add(new DiaryPanelNote("diaryRecordPanel", Model.of((NoteRecord)record)));
				}
				else
				{
					/// TODO: other diary types
					//item.add(new DiaryPanelBlood("diaryRecordPanel", record));
				}

				//				rec.add(new AjaxEventBehavior("onclick")
				//				{
				//					private static final long	serialVersionUID	= 1L;
				//
				//					@Override
				//					protected void onEvent(AjaxRequestTarget target)
				//					{
				//						Versioned<FoodItem> food = rec.getModelObject();
				//						System.out.println("Opening: " + food.getData().getName());
				//
				//						foodEditor.show(target, Model.of(food));
				//					}
				//				});
			}
		});
	}
}
