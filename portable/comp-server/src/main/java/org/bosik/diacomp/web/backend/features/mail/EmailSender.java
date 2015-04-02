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
