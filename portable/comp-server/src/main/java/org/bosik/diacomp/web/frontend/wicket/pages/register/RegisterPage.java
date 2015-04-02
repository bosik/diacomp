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
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxFallbackButton;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.HiddenField;
import org.apache.wicket.markup.html.form.PasswordTextField;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.panel.FeedbackPanel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.Url;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.request.http.WebRequest;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.EmailAddressValidator;
import org.apache.wicket.validation.validator.StringValidator;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.backend.features.mail.EmailSender;
import org.bosik.diacomp.web.backend.features.mail.SMTPEmailSender;
import org.bosik.diacomp.web.backend.features.user.auth.AuthService;
import org.bosik.diacomp.web.frontend.wicket.pages.license.LicensePage;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;
import org.json.JSONObject;

public class RegisterPage extends MasterPage
{
	private static final long	serialVersionUID	= 1L;

	@SpringBean
	AuthService					authService;

	public RegisterPage(PageParameters parameters)
	{
		super(parameters);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		final FeedbackPanel feedbackPanel = new FeedbackPanel("feedbackPanel");
		add(feedbackPanel);

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

		form.add(new BookmarkablePageLink<Void>("linkLicense", LicensePage.class));

		final WebMarkupContainer succeedPanel = new WebMarkupContainer("succeedPanel");
		succeedPanel.setOutputMarkupId(true);
		succeedPanel.setVisible(false);
		add(succeedPanel);

		final Label labelSendedEmail = new Label("labelSendedEmail", Model.of(""));
		labelSendedEmail.setOutputMarkupId(true);
		succeedPanel.add(labelSendedEmail);

		form.add(new AjaxFallbackButton("buttonRegister", form)
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target, Form<?> form)
			{
				final String antiBot = fieldFakeEmail.getModelObject();
				if (antiBot != null && !antiBot.isEmpty())
				{
					form.clearInput();
					return;
				}

				final WebRequest request = (WebRequest)RequestCycle.get().getRequest();
				final String challenge = request.getPostParameters().getParameterValue("g-recaptcha-response")
						.toString();
				final String secret = Config.get("captcha.secret");

				if (!validateCaptcha(secret, challenge))
				{
					feedbackPanel.error(getString("error.captcha"));
				}
				else
				{
					try
					{
						final String email = fieldEmail.getModelObject();
						final String password = fieldPassword.getModelObject();
						String activationKey = authService.register(email, password);
						sendActivationEmail(email, activationKey);

						form.setVisible(false);
						labelSendedEmail.setDefaultModelObject(email);
						succeedPanel.setVisible(true);
					}
					catch (MessagingException e)
					{
						feedbackPanel.error(getString("fieldEmail.EmailAddressValidator"));
					}
					catch (DuplicateException e)
					{
						feedbackPanel.error(getString("error.emailInUse"));
					}
					catch (Exception e)
					{
						e.printStackTrace();
						feedbackPanel.error(getString("error.common"));
					}
				}
			}
		});
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
		}
		return false;
	}

	void sendActivationEmail(final String email, String activationKey) throws MessagingException, AddressException,
			UnsupportedEncodingException
	{
		String hostAddress = Config.get("email.server");
		String hostUsername = Config.get("email.login");
		String hostPassword = Config.get("email.password");

		Url context = Url.parse(getRequest().getContextPath());
		String appURL = getRequestCycle().getUrlRenderer().renderFullUrl(context);

		if (!appURL.endsWith("/"))
		{
			appURL = appURL + "/";
		}

		String activationLink = String.format("%sactivate?key=%s", appURL, activationKey);
		String senderName = getString("email.sender");
		String title = getString("email.title");
		String body = String.format(getString("email.body"), activationLink, activationLink);

		EmailSender emailSender = new SMTPEmailSender(hostAddress, hostUsername, hostPassword);
		emailSender.send(title, body, new InternetAddress(hostUsername, senderName), new InternetAddress(email));
	}
}
