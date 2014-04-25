package org.bosik.diacomp.web.frontend.wicket;

import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.protocol.http.WebApplication;
import org.bosik.diacomp.web.frontend.wicket.pages.about.AboutPage;
import org.bosik.diacomp.web.frontend.wicket.pages.diary.DiaryPage;

public class WicketApplication extends WebApplication
{
	@Override
	public Class<? extends WebPage> getHomePage()
	{
		return DiaryPage.class;
	}

	@Override
	public void init()
	{
		super.init();

		mountPage("/diary", DiaryPage.class);
		mountPage("/about", AboutPage.class);
	}
}
