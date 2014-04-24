package org.bosik.diacomp.web.frontend;

import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.protocol.http.WebApplication;
import org.bosik.diacomp.web.frontend.pages.HomePage;

public class WicketApplication extends WebApplication
{
	@Override
	public Class<? extends WebPage> getHomePage()
	{
		return HomePage.class;
	}

	@Override
	public void init()
	{
		super.init();

		mountPage("/test", HomePage.class);
	}
}
