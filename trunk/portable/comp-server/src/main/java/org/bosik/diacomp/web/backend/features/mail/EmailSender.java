package org.bosik.diacomp.web.backend.features.mail;

import javax.mail.MessagingException;
import javax.mail.internet.InternetAddress;

/**
 * Email sender
 */
public interface EmailSender
{
	/**
	 * Sends e-mail
	 * 
	 * @param title
	 *            Title of email
	 * @param body
	 *            Body of email
	 * @param sender
	 *            Sender of email
	 * @param directReceivers
	 *            Email receivers
	 * @param copyReceivers
	 *            Email's copy receivers
	 * @throws MessagingException
	 *             Kinda common error
	 */
	void send(String title, String body, InternetAddress sender, InternetAddress[] directReceivers,
			InternetAddress[] copyReceivers) throws MessagingException;

	/**
	 * Sends e-mail (direct receivers only)
	 * 
	 * @param title
	 *            Title of email
	 * @param body
	 *            Body of email
	 * @param sender
	 *            Sender of email
	 * @param directReceivers
	 *            Email receivers
	 * @throws MessagingException
	 *             Kinda common error
	 */
	void send(String title, String body, InternetAddress sender, InternetAddress[] directReceivers)
			throws MessagingException;

	/**
	 * Sends e-mail (single direct receiver only)
	 * 
	 * @param title
	 *            Title of email
	 * @param body
	 *            Body of email
	 * @param sender
	 *            Sender of email
	 * @param receiver
	 *            Email receiver
	 * @throws MessagingException
	 *             Kinda common error
	 */
	void send(String title, String body, InternetAddress sender, InternetAddress receiver) throws MessagingException;
}
