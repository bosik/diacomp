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
