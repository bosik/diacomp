package org.bosik.diacomp.web.frontend.wicket;

import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.protocol.http.WebApplication;
import org.bosik.diacomp.web.frontend.wicket.pages.about.AboutPage;
import org.bosik.diacomp.web.frontend.wicket.pages.diary.DiaryPage;
import org.bosik.diacomp.web.frontend.wicket.pages.download.DownloadPage;
import org.bosik.diacomp.web.frontend.wicket.pages.foodbase.FoodBasePage;
import org.bosik.diacomp.web.frontend.wicket.pages.login.LoginPage;
import org.bosik.diacomp.web.frontend.wicket.pages.stats.StatsPage;

public class WicketApplication extends WebApplication
{
	@Override
	public Class<? extends WebPage> getHomePage()
	{
		return AboutPage.class;
	}

	@Override
	public void init()
	{
		super.init();

		mountPage("/about", AboutPage.class);
		mountPage("/login", LoginPage.class);

		mountPage("/diary", DiaryPage.class);
		mountPage("/base", FoodBasePage.class);
		mountPage("/stats", StatsPage.class);
		mountPage("/download", DownloadPage.class);
	}
}
