package org.bosik.diacomp.web.frontend.wicket.pages.login;

import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.panel.FeedbackPanel;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;
import org.bosik.diacomp.web.frontend.wicket.pages.register.RegisterPage;

public class LoginPage extends MasterPage
{
	private static final long	serialVersionUID	= 1L;

	public LoginPage(final PageParameters parameters)
	{
		super(parameters);

		add(new BookmarkablePageLink<Void>("linkRegister", RegisterPage.class));

		FeedbackPanel hint = new FeedbackPanel("hintInvalidCredentials");
		add(hint);

		if (parameters.getPosition("error") == -1)
		{
		}
		else
		{
			hint.error(getString("label.invalidCredentails"));
		}
	}
}
