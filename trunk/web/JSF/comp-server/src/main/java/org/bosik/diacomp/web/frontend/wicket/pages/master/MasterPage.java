package org.bosik.diacomp.web.frontend.wicket.pages.master;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.bosik.diacomp.web.frontend.wicket.components.header.HeaderPanel;
import org.bosik.diacomp.web.frontend.wicket.pages.diary.DiaryPage;

public class MasterPage extends WebPage
{
	private static final long	serialVersionUID	= 1L;

	public MasterPage(final PageParameters parameters)
	{
		super(parameters);

		add(new HeaderPanel("headerPanel", "user@user.com"));

		RepeatingView menu = new RepeatingView("menuItem");
		add(menu);

		for (String link : getMenuItems())
		{
			WebMarkupContainer item = new WebMarkupContainer(menu.newChildId());
			Link<Void> externalLink = new Link<Void>("Link")
			{
				private static final long	serialVersionUID	= 1L;

				@Override
				public void onClick()
				{
					// TODO
					setResponsePage(DiaryPage.class);
				}
			};
			// TODO
			externalLink.add(new Label("Text", link));
			item.add(externalLink);
			menu.add(item);
		}
	}

	protected String[] getMenuItems()
	{
		return new String[] { "Diary", "Food base", "About" };
	};
}
