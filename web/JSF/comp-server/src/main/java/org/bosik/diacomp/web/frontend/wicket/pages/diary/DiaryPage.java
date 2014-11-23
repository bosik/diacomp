package org.bosik.diacomp.web.frontend.wicket.pages.diary;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.RefreshingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.features.diary.service.FrontendDiaryService;
import org.bosik.diacomp.web.frontend.wicket.components.diary.day.DiaryPanelDay;
import org.bosik.diacomp.web.frontend.wicket.components.diary.day.DiaryPanelDayModelObject;
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

		container.add(new RefreshingView<DiaryPanelDayModelObject>("diaryDay")
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected Iterator<IModel<DiaryPanelDayModelObject>> getItemModels()
			{
				final List<IModel<DiaryPanelDayModelObject>> list = new ArrayList<IModel<DiaryPanelDayModelObject>>();

				//				FoodDataProvider provider = new FoodDataProvider();
				//				Iterator<? extends Versioned<FoodItem>> it = provider.iterator(0, 19);
				//				while (it.hasNext())
				//				{
				//					foodBase.add(provider.model(it.next()));
				//				}

				for (int i = 1; i <= 40; i++)
				{
					Date dateFrom = Utils.dateLocal(2013, 1, i);
					Date dateTo = Utils.getNextDay(dateFrom);
					List<Versioned<DiaryRecord>> day = diaryService.findPeriod(dateFrom, dateTo, false);
					DiaryPanelDayModelObject mo = new DiaryPanelDayModelObject(dateFrom, day);
					list.add(Model.of(mo));
				}
				return list.iterator();
			}

			@Override
			protected void populateItem(final Item<DiaryPanelDayModelObject> item)
			{
				item.add(new DiaryPanelDay("diaryDayPanel", item.getModelObject()));
			}
		});
	}
}
