package org.bosik.diacomp.web.frontend.wicket.pages.diary;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.RefreshingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.features.diary.service.FrontendDiaryService;
import org.bosik.diacomp.web.frontend.wicket.components.diary.blood.DiaryPanelBlood;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;

public class DiaryPage extends MasterPage
{
	private static final long			serialVersionUID	= 1L;

	WebMarkupContainer					container;
	transient static final DiaryService	diaryService		= new FrontendDiaryService();

	public DiaryPage(final PageParameters parameters)
	{
		super(parameters);

		container = new WebMarkupContainer("wrapper");
		container.setOutputMarkupId(true);
		add(container);

		container.add(new RefreshingView<Versioned<DiaryRecord>>("diaryRecord")
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

				List<Versioned<DiaryRecord>> items = diaryService.findPeriod(Utils.date(2013, 1, 1),
						Utils.date(2014, 1, 1), false);
				if (items.size() > 20)
				{
					items = items.subList(0, 20);
				}
				for (Versioned<DiaryRecord> item : items)
				{
					if (item.getData() instanceof BloodRecord)
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
					System.out.println(item.getMarkupId());
					item.add(new DiaryPanelBlood("diaryRecordPanel", (BloodRecord)record));
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
