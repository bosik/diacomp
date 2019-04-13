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
package org.bosik.diacomp.web.frontend.wicket;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.backend.features.mail.EmailSender;
import org.bosik.diacomp.web.backend.features.mail.FakeEmailSender;
import org.bosik.diacomp.web.backend.features.mail.SMTPEmailSender;
import org.json.JSONObject;

import javax.mail.MessagingException;
import javax.mail.internet.InternetAddress;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

public class Utils
{
	public static void sendEmail(String email, String title, String body, String senderName)
			throws MessagingException, UnsupportedEncodingException
	{
		String hostAddress = Config.get(Config.KEY_EMAIL_SERVER);
		String hostUsername = Config.get(Config.KEY_EMAIL_LOGIN);
		String hostPassword = Config.get(Config.KEY_EMAIL_PASSWORD);

		EmailSender emailSender = new SMTPEmailSender(hostAddress, hostUsername, hostPassword);
		emailSender.send(title, body, new InternetAddress(hostUsername, senderName), new InternetAddress(email));
	}

	public static boolean validateCaptcha(String secret, String response)
	{
		if (response == null || response.isEmpty())
		{
			return false;
		}

		try
		{
			HttpClient httpclient = HttpClients.createDefault();
			HttpPost httppost = new HttpPost("https://www.google.com/recaptcha/api/siteverify");

			List<NameValuePair> params = new ArrayList<>(2);
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
}
