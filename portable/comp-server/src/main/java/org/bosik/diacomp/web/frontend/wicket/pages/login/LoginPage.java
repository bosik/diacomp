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

import lombok.NoArgsConstructor;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.ajax.markup.html.form.AjaxFallbackButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.PasswordTextField;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.panel.FeedbackPanel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.bosik.diacomp.web.backend.features.user.auth.AuthProvider;
import org.bosik.diacomp.web.frontend.wicket.components.UpdateOnBlurBehavior;
import org.bosik.diacomp.web.frontend.wicket.pages.diary.DiaryPage;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;
import org.bosik.diacomp.web.frontend.wicket.pages.register.RegisterPage;
import org.bosik.diacomp.web.frontend.wicket.pages.restore.RestorePage;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.DisabledException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.util.StringUtils;

@NoArgsConstructor
public class LoginPage extends MasterPage
{
	@SpringBean
	private AuthProvider authService;

	private static final long   serialVersionUID = 1L;
	private              String userName         = "";
	private              String password         = "";

	public LoginPage(final PageParameters parameters)
	{
		super(parameters);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		Form<Void> form = new Form<>("formLogin");
		form.setOutputMarkupId(true);
		add(form);

		final TextField<String> fieldEmail = new TextField<>("fieldUserName", new PropertyModel<String>(this, "userName"));
		fieldEmail.add(new UpdateOnBlurBehavior());
		form.add(fieldEmail);

		final PasswordTextField fieldPassword = new PasswordTextField("fieldPassword", new PropertyModel<String>(this, "password"));
		fieldPassword.add(new UpdateOnBlurBehavior());
		fieldPassword.setRequired(false); // to handle it manually
		form.add(fieldPassword);

		form.add(new BookmarkablePageLink<Void>("linkRegister", RegisterPage.class));
		form.add(new AjaxFallbackLink<Void>("linkRestore")
		{
			@Override
			public void onClick(AjaxRequestTarget ajaxRequestTarget)
			{
				PageParameters params = new PageParameters();
				params.set("email", userName);
				setResponsePage(new RestorePage(params));
			}
		});

		final FeedbackPanel hint = new FeedbackPanel("hintInvalidCredentials");
		hint.setOutputMarkupId(true);
		add(hint);

		AjaxFallbackButton buttonRegister = new AjaxFallbackButton("buttonLogin", form)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target, Form<?> form)
			{
				try
				{
					if (StringUtils.isEmpty(userName))
					{
						showError(target, getString("error.userName.empty"));
						return;
					}

					if (StringUtils.isEmpty(password))
					{
						showError(target, getString("error.password.empty"));
						return;
					}

					userName = userName.toLowerCase();

					Authentication auth = authService.authenticate(new UsernamePasswordAuthenticationToken(userName, password));
					SecurityContext context = SecurityContextHolder.getContext();
					context.setAuthentication(auth);
					LoginPage.this.setResponsePage(DiaryPage.class);
				}
				catch (UsernameNotFoundException e)
				{
					showError(target, getString("error.userNotFound"));
				}
				catch (DisabledException e)
				{
					showError(target, getString("error.userNotActivated"));
				}
				catch (BadCredentialsException e)
				{
					showError(target, getString("error.badCredentials"));
				}
				catch (Exception e)
				{
					e.printStackTrace();
					showError(target, getString("error.common"));
				}
			}

			private void showError(AjaxRequestTarget target, String msg)
			{
				error(msg);
				target.add(hint);
			}
		};
		buttonRegister.setOutputMarkupId(true);
		form.add(buttonRegister);
	}

}
