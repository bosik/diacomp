package org.bosik.diacomp.web.frontend.wicket.pages;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.bosik.diacomp.web.frontend.wicket.components.header.Header;

public class HomePage extends WebPage
{
	private static final long	serialVersionUID	= 1L;

	private final Label			secondLabel;

	public HomePage(final PageParameters parameters)
	{
		super(parameters);

		add(new AjaxFallbackLink<Void>("foodbase")
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				secondLabel.setVisible(true);
				target.add(secondLabel);
			}
		});
		secondLabel = new Label("second", "The second label");
		secondLabel.setOutputMarkupPlaceholderTag(true);
		secondLabel.setVisible(false);
		add(secondLabel);

		add(new Header("headerPanel", "user@user.com"));
	}
}
