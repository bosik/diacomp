/*  
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *  
 */
package org.bosik.diacomp.android.utils;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import org.bosik.diacomp.android.BuildConfig;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.android.frontend.UIUtils;
import android.app.Activity;
import android.util.Log;

public class ErrorHandler
{
	private static final String				TAG						= ErrorHandler.class.getSimpleName();
	private static WebClient				webClient;

	// Messages
	private static final String				TIP_ERROR_OCCURED		= "Произошла ошибка";
	private static final String				TIP_REPORT_SENDED		= "Отчёт с технической информацией отправлен разработчику";
	private static final String				TIP_REPORT_FAILED		= "Отправить отчёт не удалось";
	private static final String				TIP_REPORT_FAILED_NULL	= "Отправить отчёт не удалось (webClient == null)";
	private static final String				TIP_HANDLING_FAILED		= "Во время обработки ошибки произошла ещё одна ошибка :(";

	// Formats
	private static final SimpleDateFormat	DATE_FORMAT				= new SimpleDateFormat("dd.MM.yyyy HH:mm:ss",
																			Locale.US);

	public static void init(WebClient webClient)
	{
		if (webClient == null)
		{
			throw new IllegalArgumentException("Client is null");
		}
		ErrorHandler.webClient = webClient;
	}

	private static String safe(String s)
	{
		return (s != null ? s : "null");
	}

	public static void handle(Throwable e, Activity activity)
	{
		if (BuildConfig.DEBUG)
		{
			if (e != null)
			{
				Log.e(TAG, "Error handler [mode: debug]: " + e.getLocalizedMessage());
				throw new RuntimeException(e);
			}
			else
			{
				Log.e(TAG, "Error handler [mode: debug]: (e == null)");
				throw new RuntimeException();
			}
		}
		else
		{
			try
			{
				if (e != null)
				{
					Log.e(TAG, "Error handler [mode: release]: " + e.getLocalizedMessage());
				}
				else
				{
					Log.e(TAG, "Error handler [mode: release]: (e == null)");
				}

				if (activity != null)
				{
					UIUtils.showTip(activity, TIP_ERROR_OCCURED);
				}

				String trace = (e != null ? Log.getStackTraceString(e) : "(e == null)");
				String exc = (e != null ? e.getLocalizedMessage() : "(e == null)");

				StringBuilder msg = new StringBuilder();
				msg.append("<b>" + TAG + " has detected exception during user runtime</b>\n\n");
				msg.append("<b>Date:</b> " + DATE_FORMAT.format(new Date()) + "\n");
				msg.append("<b>User:</b> "
						+ (webClient != null ? safe(webClient.getUsername()) : "(webClient == null)") + "\n");
				msg.append("<b>Exception:</b>" + exc + "\n\n");
				msg.append("<font name='Courier New'>");
				msg.append(trace);
				msg.append("</font>");

				String m = msg.toString();
				Log.e(TAG, m);

				if (webClient != null)
				{
					boolean sent;
					try
					{
						webClient.sendMail(m.replace("\n", "<br/>"));
						sent = true;
					}
					catch (Exception ex)
					{
						sent = false;
					}

					if (activity != null)
					{
						if (sent)
						{
							UIUtils.showTip(activity, TIP_REPORT_SENDED);
						}
						else
						{
							UIUtils.showTip(activity, TIP_REPORT_FAILED);
						}
					}
				}
				else
				{
					if (activity != null)
					{
						UIUtils.showTip(activity, TIP_REPORT_FAILED_NULL);
					}
				}
			}
			catch (Exception ex)
			{
				UIUtils.showTip(activity, TIP_HANDLING_FAILED);
				// well, something bad has occurred and we have no way to talk about it :(
				// ok, let's print it for some kind of smart user
				ex.printStackTrace();
			}
		}
	}

	public static void handle(Throwable e)
	{
		handle(e, null);
	}
}
