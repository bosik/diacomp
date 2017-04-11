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

import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;
import javax.mail.MessagingException;
import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
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
import org.apache.wicket.validation.validator.StringValidator;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.backend.features.mail.EmailSender;
import org.bosik.diacomp.web.backend.features.mail.SMTPEmailSender;
import org.bosik.diacomp.web.backend.features.user.auth.AuthService;
import org.bosik.diacomp.web.frontend.wicket.pages.license.eula.EulaPage;
import org.bosik.diacomp.web.frontend.wicket.pages.license.privacy.PrivacyPolicyPage;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;
import org.bosik.diacomp.web.frontend.wicket.pages.register.succeed.RegistrationSucceedPage;
import org.json.JSONObject;

public class RegisterPage extends MasterPage
{
	private static final long		serialVersionUID	= 1L;

	@SpringBean
	AuthService						authService;

	FeedbackPanel					feedbackPanel;
	AjaxFallbackButton				buttonRegister;
	WebMarkupContainer				progressSpinner;
	AjaxSelfUpdatingTimerBehavior	progressBehavior;
	Progress						progress;

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
						Page succeedPage = new RegistrationSucceedPage(Model.of(progress.getMessage()));
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

		Form<Void> form = new Form<Void>("regForm");
		form.setOutputMarkupId(true);
		add(form);

		final TextField<String> fieldEmail = new TextField<String>("rhCyIStebK", Model.of(""));
		fieldEmail.add(EmailAddressValidator.getInstance());
		fieldEmail.setRequired(true);
		form.add(fieldEmail);

		final HiddenField<String> fieldFakeEmail = new HiddenField<String>("email", Model.of(""));
		form.add(fieldFakeEmail);

		final PasswordTextField fieldPassword = new PasswordTextField("P2BohS6rUR", Model.of(""));
		fieldPassword.setRequired(true);
		fieldPassword.add(StringValidator.minimumLength(6));
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
				final String challenge = request.getPostParameters().getParameterValue("g-recaptcha-response")
						.toString();
				final String secret = Config.get(Config.KEY_CAPTCHA_SECRET);

				final String MSG_ERROR_CAPTCHA = RegisterPage.this.getString("error.captcha");
				final String MSG_ERROR_EMAIL = RegisterPage.this.getString("error.wrongEmail");
				final String MSG_ERROR_DUPLICATION = RegisterPage.this.getString("error.emailInUse");
				final String MSG_ERROR_COMON = RegisterPage.this.getString("error.common");
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
							progress = new Progress();
							progress.setRunning(true);

							if (!validateCaptcha(secret, challenge))
							{
								progress.fail(MSG_ERROR_CAPTCHA);
								return;
							}

							String email = fieldEmail.getModelObject();
							String password = fieldPassword.getModelObject();
							String activationKey = authService.register(email, password);

							String activationLink = String.format("%sactivate?key=%s", appUrl, activationKey);
							String body = String.format(bodyPattern, activationLink, activationLink);
							sendActivationEmail(email, title, body, sender);

							progress.success(email);
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
							progress.fail(MSG_ERROR_COMON);
						}
					}
				}.start();
			}
		};
		buttonRegister.setOutputMarkupId(true);
		form.add(buttonRegister);
	}

	protected static boolean validateCaptcha(String secret, String response)
	{
		if (response == null || response.isEmpty())
		{
			return false;
		}

		try
		{
			HttpClient httpclient = HttpClients.createDefault();
			HttpPost httppost = new HttpPost("https://www.google.com/recaptcha/api/siteverify");

			List<NameValuePair> params = new ArrayList<NameValuePair>(2);
			params.add(new BasicNameValuePair("secret", secret));
			params.add(new BasicNameValuePair("response", response));
			httppost.setEntity(new UrlEncodedFormEntity(params, "UTF-8"));

			HttpResponse httpResponse = httpclient.execute(httppost);
			HttpEntity entity = httpResponse.getEntity();

			if (entity != null)
			{
				String content = EntityUtils.toString(entity);

				JSONObject json = new JSONObject(content);
				boolean success = json.getBoolean("success");
				return success;
			}
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		return false;
	}

	static void sendActivationEmail(String email, String title, String body, String senderName)
			throws MessagingException, AddressException, UnsupportedEncodingException
	{
		String hostAddress = Config.get(Config.KEY_EMAIL_SERVER);
		String hostUsername = Config.get(Config.KEY_EMAIL_LOGIN);
		String hostPassword = Config.get(Config.KEY_EMAIL_PASSWORD);

		EmailSender emailSender = new SMTPEmailSender(hostAddress, hostUsername, hostPassword);
		emailSender.send(title, body, new InternetAddress(hostUsername, senderName), new InternetAddress(email));
	}
}

class Progress implements Serializable
{
	private static final long	serialVersionUID	= 1L;

	private boolean				running;
	private String				message;
	private boolean				success;

	public synchronized void success(String message)
	{
		this.running = false;
		this.message = message;
		this.success = true;
	}

	public synchronized void fail(String message)
	{
		this.running = false;
		this.message = message;
		this.success = false;
	}

	public boolean isRunning()
	{
		return running;
	}

	public void setRunning(boolean running)
	{
		this.running = running;
	}

	public String getMessage()
	{
		return message;
	}

	public void setMessage(String message)
	{
		this.message = message;
	}

	public boolean isSuccess()
	{
		return success;
	}

	public void setSuccess(boolean success)
	{
		this.success = success;
	}
}
