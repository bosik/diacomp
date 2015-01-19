package org.bosik.diacomp.web.frontend.wicket.pages.login;

import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;

public class LoginPage extends MasterPage
{
	private static final long	serialVersionUID	= 1L;

	public LoginPage(final PageParameters parameters)
	{
		super(parameters);

		// TODO: implement UI feedback
		if (parameters.getPosition("error") == -1)
		{
			System.out.println("First try");
		}
		else
		{
			System.out.println("Bad credentials");
		}
	}
}
