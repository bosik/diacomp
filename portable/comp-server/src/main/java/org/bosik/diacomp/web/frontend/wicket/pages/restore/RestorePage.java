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
package org.bosik.diacomp.web.frontend.wicket.pages.restore;

import org.apache.wicket.Page;
import org.apache.wicket.Session;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.AjaxSelfUpdatingTimerBehavior;
import org.apache.wicket.ajax.markup.html.form.AjaxFallbackButton;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.HiddenField;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.FeedbackPanel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.request.http.WebRequest;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.string.StringValue;
import org.apache.wicket.util.time.Duration;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.backend.features.user.auth.AuthService;
import org.bosik.diacomp.web.backend.features.user.auth.validation.Validator;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.UserNameIsEmptyException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.ValidationException;
import org.bosik.diacomp.web.frontend.wicket.ProgressBundle;
import org.bosik.diacomp.web.frontend.wicket.Utils;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;
import org.bosik.diacomp.web.frontend.wicket.pages.restore.sent.RestoreSentPage;

import javax.mail.MessagingException;

public class RestorePage extends MasterPage
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private AuthService authService;

	private FeedbackPanel                 feedbackPanel;
	private AjaxFallbackButton            buttonRestore;
	private WebMarkupContainer            progressSpinner;
	private AjaxSelfUpdatingTimerBehavior progressBehavior;
	private ProgressBundle                progress;

	public RestorePage(PageParameters parameters)
	{
		super(parameters);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		final StringValue parKey = getPageParameters().get("email");
		final String defaultEmail = parKey.toString("");

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
					buttonRestore.setEnabled(true);
					target.add(buttonRestore);

					if (progress.isSuccess())
					{
						Page succeedPage = new RestoreSentPage(Model.of(progress.getMessage()));
						setResponsePage(succeedPage);
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

		Form<Void> form = new Form<>("restoreForm");
		form.setOutputMarkupId(true);
		add(form);

		final TextField<String> fieldEmail = new TextField<>("siDyIVtwLp", Model.of(defaultEmail));
		form.add(fieldEmail);

		buttonRestore = new AjaxFallbackButton("buttonRestore", form)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target, Form<?> form)
			{
				final WebRequest request = (WebRequest) RequestCycle.get().getRequest();
				final String challenge = request.getPostParameters().getParameterValue("g-recaptcha-response").toString();
				final String secret = Config.get(Config.KEY_CAPTCHA_SECRET);

				final String MSG_ERROR_CAPTCHA = getString("error.captcha");
				final String MSG_ERROR_EMAIL = getString("error.wrongEmail");
				final String MSG_ERROR_UNKNOWN_EMAIL = getString("error.unknownEmail");
				final String MSG_ERROR_USERNAME_IS_EMPTY = getString("error.userName.empty");
				final String MSG_ERROR_COMMON = getString("error.common");
				final String appUrlRaw = Config.get(Config.KEY_APP_URL);
				final String appUrl = appUrlRaw.endsWith("/") ? appUrlRaw : appUrlRaw + "/";
				final String bodyPattern = getString("email.body");
				final String title = getString("email.title");
				final String sender = getString("email.sender");

				progressBehavior.restart(target);
				buttonRestore.setEnabled(false);
				progressSpinner.setVisible(true);
				Session.get().getFeedbackMessages().clear();
				target.add(buttonRestore, progressSpinner, feedbackPanel);

				new Thread()
				{
					@Override
					public void run()
					{
						try
						{
							progress = new ProgressBundle();
							progress.setRunning(true);

							String email = fieldEmail.getModelObject();
							Validator.validateUserName(email);

							if (!Utils.validateCaptcha(secret, challenge))
							{
								progress.fail(MSG_ERROR_CAPTCHA);
								return;
							}

							String restoreKey = authService.buildRestoreKey(email);

							if (restoreKey != null)
							{
								String restoreLink = String.format("%srestore/change?key=%s", appUrl, restoreKey);
								String body = String.format(bodyPattern, restoreLink, restoreLink);
								Utils.sendEmail(email, title, body, sender);

								progress.success(email);
							}
							else
							{
								progress.fail(MSG_ERROR_UNKNOWN_EMAIL);
							}
						}
						catch (UserNameIsEmptyException e)
						{
							progress.fail(MSG_ERROR_USERNAME_IS_EMPTY);
						}
						catch (ValidationException e)
						{
							progress.fail(MSG_ERROR_UNKNOWN_EMAIL);
						}
						catch (MessagingException e)
						{
							progress.fail(MSG_ERROR_EMAIL);
						}
						catch (Exception e)
						{
							e.printStackTrace();
							progress.fail(MSG_ERROR_COMMON);
						}
						finally
						{
							progress.setRunning(false);
						}
					}
				}.start();
			}
		};
		buttonRestore.setOutputMarkupId(true);
		form.add(buttonRestore);
	}
}

