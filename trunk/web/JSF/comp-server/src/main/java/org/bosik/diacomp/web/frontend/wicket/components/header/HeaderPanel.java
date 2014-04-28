package org.bosik.diacomp.web.frontend.wicket.components.header;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.panel.Panel;
import org.bosik.diacomp.core.services.AuthService;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.frontend.features.auth.AuthRestClient;
import org.bosik.diacomp.web.frontend.wicket.pages.diary.DiaryPage;

public class HeaderPanel extends Panel
{
	private static final long	serialVersionUID	= 1L;

	AuthService					authService			= new AuthRestClient();

	public HeaderPanel(String id, String userName)
	{
		super(id);
		String userName2;
		try
		{
			userName2 = authService.getUserName();
		}
		catch (NotAuthorizedException e)
		{
			userName2 = "";
		}
		add(new Label("infoLogin", userName2));
		//		add(new Link<Void>("linkLogout")
		//		{
		//			private static final long	serialVersionUID	= 1L;
		//
		//			@Override
		//			public void onClick()
		//			{
		//				authService.logout();
		//				// TODO: setResponsePage(LoginPage.class);
		//			}
		//		});

		add(new BookmarkablePageLink<Void>("linkHome", DiaryPage.class));
		add(new AjaxFallbackLink<Void>("linkLogout")
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				authService.logout();
				// TODO: setResponsePage(LoginPage.class);
			}
		});
	}
}
