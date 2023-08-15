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
package org.bosik.diacomp.web.backend.features.mail;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import java.util.Properties;

/**
 * Sends email via SMTP<br/>
 * Encoding: utf-8<br/>
 * Content Type: text/html
 */
public class SMTPEmailSender extends AbstractEmailSender
{
	private String	hostAddress;
	String			hostUsername;
	String			hostPassword;

	public SMTPEmailSender(String hostAddress, String hostUsername, String hostPassword)
	{
		this.hostAddress = hostAddress;
		this.hostUsername = hostUsername;
		this.hostPassword = hostPassword;
	}

	@Override
	public void send(String title, String body, InternetAddress sender, InternetAddress[] directReceivers,
			InternetAddress[] copyReceivers) throws MessagingException
	{
		Properties props = System.getProperties();
		props.put("mail.smtp.host", hostAddress);
		props.put("mail.smtp.auth", "true");
		props.put("mail.smtp.socketFactory.port", "465");
		props.put("mail.smtp.socketFactory.class", "javax.net.ssl.SSLSocketFactory");

		Session session = Session.getDefaultInstance(props, new javax.mail.Authenticator()
		{
			@Override
			protected PasswordAuthentication getPasswordAuthentication()
			{
				return new PasswordAuthentication(hostUsername, hostPassword);
			}
		});

		MimeMessage message = new MimeMessage(session);
		message.setSubject(title);
		message.setText(body);
		message.setHeader("Content-Type", "text/html;charset=utf-8");
		message.setFrom(sender);
		message.addRecipients(Message.RecipientType.TO, directReceivers);
		message.addRecipients(Message.RecipientType.CC, copyReceivers);

		Transport.send(message, message.getAllRecipients());
		// SMTPTransport transport = (SMTPTransport)
		// session.getTransport("smtp");
		// transport.setStartTLS(true);
		// transport.connect(hostAddress, hostUsername, hostPassword);
		// transport.sendMessage(message, message.getAllRecipients());
	}
}
