package org.bosik.diacomp.web.frontend.wicket.pages.master;

import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
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

		add(new BookmarkablePageLink<Void>("menuItemDiary", DiaryPage.class));
		add(new BookmarkablePageLink<Void>("menuItemBase", DiaryPage.class)); // TODO
		add(new BookmarkablePageLink<Void>("menuItemAbout", DiaryPage.class)); // TODO
		add(new BookmarkablePageLink<Void>("menuItemDownload", DiaryPage.class)); // TODO
	}
}
