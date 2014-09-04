package org.bosik.diacomp.web.frontend.wicket.components.header;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.html.panel.Panel;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.backend.features.auth.service.AuthService;
import org.bosik.diacomp.web.backend.features.auth.service.FrontendAuthService;
import org.bosik.diacomp.web.frontend.wicket.pages.about.AboutPage;
import org.bosik.diacomp.web.frontend.wicket.pages.diary.DiaryPage;

public class HeaderPanel extends Panel
{
	private static final long	serialVersionUID	= 1L;

	private final AuthService	authService			= new FrontendAuthService();

	public HeaderPanel(String id, String userName_unused)
	{
		super(id);
		try
		{
			String userName = authService.getCurrentUserName();
			add(new ExternalLink("linkLogout", "j_spring_security_logout"));
			add(new BookmarkablePageLink<Void>("linkHome", DiaryPage.class));
			add(new Label("infoLogin", userName));
		}
		catch (NotAuthorizedException e)
		{
			add(new ExternalLink("linkLogout", "j_spring_security_logout").setVisible(false));
			add(new BookmarkablePageLink<Void>("linkHome", AboutPage.class));
			add(new Label("infoLogin"));
		}
	}
}
