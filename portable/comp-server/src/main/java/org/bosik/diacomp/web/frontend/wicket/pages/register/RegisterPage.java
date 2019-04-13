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
package org.bosik.diacomp.web.frontend.wicket.pages.register;

import org.apache.wicket.Page;
import org.apache.wicket.Session;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.AjaxSelfUpdatingTimerBehavior;
import org.apache.wicket.ajax.markup.html.form.AjaxFallbackButton;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.HiddenField;
import org.apache.wicket.markup.html.form.PasswordTextField;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.panel.FeedbackPanel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.request.http.WebRequest;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.time.Duration;
import org.apache.wicket.validation.validator.EmailAddressValidator;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.backend.features.user.auth.AuthService;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.PasswordIsEmptyException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.PasswordTooLongException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.PasswordTooShortException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.UserNameIsEmptyException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.UserNameTooLongException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.exceptions.UserNameTooShortException;
import org.bosik.diacomp.web.frontend.wicket.ProgressBundle;
import org.bosik.diacomp.web.frontend.wicket.Utils;
import org.bosik.diacomp.web.frontend.wicket.pages.license.eula.EulaPage;
import org.bosik.diacomp.web.frontend.wicket.pages.license.privacy.PrivacyPolicyPage;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;
import org.bosik.diacomp.web.frontend.wicket.pages.register.sent.RegistrationSentPage;
import org.springframework.util.StringUtils;

import javax.mail.MessagingException;

public class RegisterPage extends MasterPage
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private AuthService authService;

	private FeedbackPanel                 feedbackPanel;
	private AjaxFallbackButton            buttonRegister;
	private WebMarkupContainer            progressSpinner;
	private AjaxSelfUpdatingTimerBehavior progressBehavior;
	private ProgressBundle                progress;

	public RegisterPage(PageParameters parameters)
	{
		super(parameters);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

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
					buttonRegister.setEnabled(true);
					target.add(buttonRegister);

					if (progress.isSuccess())
					{
						Page succeedPage = new RegistrationSentPage(Model.of(progress.getMessage()));
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

		Form<Void> form = new Form<>("regForm");
		form.setOutputMarkupId(true);
		add(form);

		final TextField<String> fieldEmail = new TextField<>("rhCyIStebK", Model.of(""));
		fieldEmail.add(EmailAddressValidator.getInstance());
		form.add(fieldEmail);

		final HiddenField<String> fieldFakeEmail = new HiddenField<>("email", Model.of(""));
		form.add(fieldFakeEmail);

		final PasswordTextField fieldPassword = new PasswordTextField("P2BohS6rUR", Model.of(""));
		form.add(fieldPassword);

		form.add(new BookmarkablePageLink<Void>("linkEula", EulaPage.class));
		form.add(new BookmarkablePageLink<Void>("linkPrivacy", PrivacyPolicyPage.class));

		buttonRegister = new AjaxFallbackButton("buttonRegister", form)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target, Form<?> form)
			{
				final String antiBot = fieldFakeEmail.getModelObject();
				if (antiBot != null && !antiBot.isEmpty())
				{
					form.clearInput();
					return;
				}

				final WebRequest request = (WebRequest) RequestCycle.get().getRequest();
				final String challenge = request.getPostParameters().getParameterValue("g-recaptcha-response").toString();
				final String secret = Config.get(Config.KEY_CAPTCHA_SECRET);

				final String MSG_ERROR_CAPTCHA = getString("error.captcha");
				final String MSG_ERROR_EMAIL = getString("error.wrongEmail");
				final String MSG_ERROR_DUPLICATION = getString("error.emailInUse");
				final String MSG_ERROR_PASSWORD_IS_EMPTY = getString("error.password.empty");
				final String MSG_ERROR_PASSWORD_TOO_SHORT = getString("error.password.tooShort");
				final String MSG_ERROR_PASSWORD_TOO_LONG = getString("error.password.tooLong");
				final String MSG_ERROR_USERNAME_IS_EMPTY = getString("error.userName.empty");
				final String MSG_ERROR_USERNAME_TOO_SHORT = getString("error.userName.tooShort");
				final String MSG_ERROR_USERNAME_TOO_LONG = getString("error.userName.tooLong");
				final String MSG_ERROR_COMMON = getString("error.common");

				final String appUrlRaw = Config.get(Config.KEY_APP_URL);
				final String appUrl = appUrlRaw.endsWith("/") ? appUrlRaw : appUrlRaw + "/";
				final String bodyPattern = getString("email.body");
				final String title = getString("email.title");
				final String sender = getString("email.sender");

				progressBehavior.restart(target);
				buttonRegister.setEnabled(false);
				progressSpinner.setVisible(true);
				Session.get().getFeedbackMessages().clear();
				target.add(buttonRegister, progressSpinner, feedbackPanel);

				new Thread()
				{
					@Override
					public void run()
					{
						try
						{
							progress = new ProgressBundle();
							progress.setRunning(true);

							if (!Utils.validateCaptcha(secret, challenge))
							{
								progress.fail(MSG_ERROR_CAPTCHA);
								return;
							}

							String email = fieldEmail.getModelObject();
							String password = fieldPassword.getModelObject();

							if (!StringUtils.isEmpty(email))
							{
								email = email.toLowerCase();
							}

							String activationKey = authService.register(email, password);
							String activationLink = String.format("%sregister/activate?key=%s", appUrl, activationKey);
							String body = String.format(bodyPattern, activationLink, activationLink);
							Utils.sendEmail(email, title, body, sender);

							progress.success(email);
						}
						catch (UserNameIsEmptyException e)
						{
							progress.fail(MSG_ERROR_USERNAME_IS_EMPTY);
						}
						catch (UserNameTooShortException e)
						{
							progress.fail(MSG_ERROR_USERNAME_TOO_SHORT);
						}
						catch (UserNameTooLongException e)
						{
							progress.fail(MSG_ERROR_USERNAME_TOO_LONG);
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
						catch (MessagingException e)
						{
							progress.fail(MSG_ERROR_EMAIL);
						}
						catch (DuplicateException e)
						{
							progress.fail(MSG_ERROR_DUPLICATION);
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
		buttonRegister.setOutputMarkupId(true);
		form.add(buttonRegister);
	}
}