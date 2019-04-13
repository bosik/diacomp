/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.web.frontend.wicket.pages.login;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.panel.FeedbackPanel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;
import org.bosik.diacomp.web.frontend.wicket.pages.register.RegisterPage;
import org.bosik.diacomp.web.frontend.wicket.pages.restore.RestorePage;

public class LoginPage extends MasterPage
{
	private static final long   serialVersionUID = 1L;
	private              String userName         = "";

	public LoginPage(final PageParameters parameters)
	{
		super(parameters);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		final TextField<String> fieldEmail = new TextField<>("userName", new PropertyModel<String>(this, "userName"));
		fieldEmail.add(new AjaxFormComponentUpdatingBehavior("onblur")
		{
			private static final long serialVersionUID = 1072515919159765189L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
			}
		});
		add(fieldEmail);

		add(new BookmarkablePageLink<Void>("linkRegister", RegisterPage.class));
		add(new AjaxFallbackLink<Void>("linkRestore")
		{
			@Override
			public void onClick(AjaxRequestTarget ajaxRequestTarget)
			{
				PageParameters params = new PageParameters();
				params.set("email", userName);
				setResponsePage(new RestorePage(params));
			}
		});

		FeedbackPanel hint = new FeedbackPanel("hintInvalidCredentials");
		add(hint);

		if (getPageParameters().getPosition("error") != -1)
		{
			hint.error(getString("label.invalidCredentials"));
		}
	}
}
