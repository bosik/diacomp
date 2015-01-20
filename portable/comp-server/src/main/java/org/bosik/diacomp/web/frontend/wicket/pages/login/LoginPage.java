package org.bosik.diacomp.web.frontend.wicket.pages.login;

import org.apache.wicket.markup.html.panel.FeedbackPanel;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;

public class LoginPage extends MasterPage
{
	private static final long	serialVersionUID	= 1L;

	public LoginPage(final PageParameters parameters)
	{
		super(parameters);

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
