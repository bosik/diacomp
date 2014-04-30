package org.bosik.diacomp.web.frontend.wicket.components.header;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.html.panel.Panel;
import org.bosik.diacomp.core.services.AuthService;
import org.bosik.diacomp.web.frontend.features.auth.AuthRestClient;
import org.bosik.diacomp.web.frontend.wicket.pages.about.AboutPage;
import org.bosik.diacomp.web.frontend.wicket.pages.diary.DiaryPage;

public class HeaderPanel extends Panel
{
	private static final long	serialVersionUID	= 1L;

	AuthService					authService			= new AuthRestClient();

	public HeaderPanel(String id, String userName_unused)
	{
		super(id);
		String userName = authService.getUserName();

		if (userName == null)
		{
			add(new ExternalLink("linkLogout", "j_spring_security_logout").setVisible(false));
			add(new BookmarkablePageLink<Void>("linkHome", AboutPage.class));
			add(new Label("infoLogin"));
		}
		else
		{
			add(new ExternalLink("linkLogout", "j_spring_security_logout"));
			add(new BookmarkablePageLink<Void>("linkHome", DiaryPage.class));
			add(new Label("infoLogin", userName));
		}
	}
}
