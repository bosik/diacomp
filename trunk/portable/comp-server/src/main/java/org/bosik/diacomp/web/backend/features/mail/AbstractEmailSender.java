package org.bosik.diacomp.web.backend.features.mail;

import javax.mail.MessagingException;
import javax.mail.internet.InternetAddress;

/**
 * Implements some simple methods by invoking complicated one (adding necessary parameters automatically).
 * 
 */
public abstract class AbstractEmailSender implements EmailSender
{
	@Override
	public void send(String title, String body, InternetAddress sender, InternetAddress[] directReceivers)
			throws MessagingException
	{
		send(title, body, sender, directReceivers, new InternetAddress[] {});
	}

	@Override
	public void send(String title, String body, InternetAddress sender, InternetAddress receiver)
			throws MessagingException
	{
		send(title, body, sender, new InternetAddress[] { receiver }, new InternetAddress[] {});
	}
}
