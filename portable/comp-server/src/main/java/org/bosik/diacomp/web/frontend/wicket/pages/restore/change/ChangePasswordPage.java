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
package org.bosik.diacomp.web.frontend.wicket.pages.restore.change;

import org.apache.wicket.Session;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.AjaxSelfUpdatingTimerBehavior;
import org.apache.wicket.ajax.markup.html.form.AjaxFallbackButton;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.PasswordTextField;
import org.apache.wicket.markup.html.panel.FeedbackPanel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.string.StringValue;
import org.apache.wicket.util.time.Duration;
import org.bosik.diacomp.web.backend.features.user.auth.AuthService;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.PasswordIsEmptyException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.PasswordTooLongException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.PasswordTooShortException;
import org.bosik.diacomp.web.frontend.wicket.ProgressBundle;
import org.bosik.diacomp.web.frontend.wicket.pages.diary.DiaryPage;
import org.bosik.diacomp.web.frontend.wicket.pages.login.LoginPage;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;

public class ChangePasswordPage extends MasterPage
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private AuthService authService;

	private FeedbackPanel                 feedbackPanel;
	private AjaxFallbackButton            buttonSave;
	private WebMarkupContainer            progressSpinner;
	private AjaxSelfUpdatingTimerBehavior progressBehavior;
	private ProgressBundle                progress;

	public ChangePasswordPage(PageParameters parameters)
	{
		super(parameters);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		StringValue parKey = getPageParameters().get("key");
		if (parKey.isEmpty())
		{
			setResponsePage(getApplication().getHomePage());
		}

		final String key = parKey.toString();

		feedbackPanel = new FeedbackPanel("feedbackPanel");
		feedbackPanel.setOutputMarkupId(true);
		add(feedbackPanel);

		final WebMarkupContainer progressContainer = new WebMarkupContainer("progressContainer");
		progressContainer.setOutputMarkupId(true);
		add(progressContainer);

		progressBehavior = new AjaxSelfUpdatingTimerBehavior(Duration.seconds(1))
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onPostProcessTarget(AjaxRequestTarget target)
			{
				if (progress.isRunning())
				{
					progressSpinner.setVisible(true);
				}
				else
				{
					progressSpinner.setVisible(false);
					progressBehavior.stop(null);
					buttonSave.setEnabled(true);
					target.add(buttonSave);

					if (progress.isSuccess())
					{
						setResponsePage(LoginPage.class);
					}
					else
					{
						feedbackPanel.error(progress.getMessage());
						target.add(feedbackPanel);
					}
				}
			}
		};
		progressBehavior.stop(null);
		progressContainer.add(progressBehavior);

		progressSpinner = new WebMarkupContainer("progressSpinner");
		progressSpinner.setOutputMarkupId(true);
		progressSpinner.setVisible(false);
		progressContainer.add(progressSpinner);

		Form<Void> form = new Form<>("resetForm");
		form.setOutputMarkupId(true);
		add(form);

		final PasswordTextField fieldPassword = new PasswordTextField("82IGBs83hF", Model.of(""));
		fieldPassword.setRequired(false); // to handle it manually
		form.add(fieldPassword);

		buttonSave = new AjaxFallbackButton("buttonChangePassword", form)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target, Form<?> form)
			{
				final String MSG_ERROR_COMMON = getString("error.common");
				final String MSG_ERROR_WRONG_KEY = getString("error.key.wrong");
				final String MSG_ERROR_PASSWORD_IS_EMPTY = getString("error.password.empty");
				final String MSG_ERROR_PASSWORD_TOO_SHORT = getString("error.password.tooShort");
				final String MSG_ERROR_PASSWORD_TOO_LONG = getString("error.password.tooLong");

				progressBehavior.restart(target);
				buttonSave.setEnabled(false);
				progressSpinner.setVisible(true);
				Session.get().getFeedbackMessages().clear();
				target.add(buttonSave, progressSpinner, feedbackPanel);

				new Thread()
				{
					@Override
					public void run()
					{
						try
						{
							progress = new ProgressBundle();
							progress.setRunning(true);

							String password = fieldPassword.getModelObject();
							authService.changePassword(key, password);

							progress.success("");
						}
						catch (PasswordIsEmptyException e)
						{
							progress.fail(MSG_ERROR_PASSWORD_IS_EMPTY);
						}
						catch (PasswordTooShortException e)
						{
							progress.fail(MSG_ERROR_PASSWORD_TOO_SHORT);
						}
						catch (PasswordTooLongException e)
						{
							progress.fail(MSG_ERROR_PASSWORD_TOO_LONG);
						}
						catch (IllegalArgumentException e)
						{
							progress.fail(MSG_ERROR_WRONG_KEY);
						}
						catch (Exception e)
						{
							e.printStackTrace();
							progress.fail(MSG_ERROR_COMMON);
						}
					}
				}.start();
			}
		};
		buttonSave.setOutputMarkupId(true);
		form.add(buttonSave);
	}
}
