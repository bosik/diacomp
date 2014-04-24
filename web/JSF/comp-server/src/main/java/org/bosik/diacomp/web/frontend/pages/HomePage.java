package org.bosik.diacomp.web.frontend.pages;

import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.request.mapper.parameter.PageParameters;

public class HomePage extends WebPage
{
	private static final long	serialVersionUID	= 1L;

	public HomePage(final PageParameters parameters)
	{
		super(parameters);

		add(new Link<Void>("foodbase")
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			public void onClick()
			{

			}
		});

		//add(new BookmarkablePageLink<Void>("foodbase", FoodbasePage.class));
	}
}
