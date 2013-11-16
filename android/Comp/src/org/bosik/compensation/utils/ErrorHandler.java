package org.bosik.compensation.utils;

import org.bosik.compensation.face.BuildConfig;
import org.bosik.compensation.face.UIUtils;
import org.bosik.compensation.persistence.repository.providers.WebClient;
import android.app.Activity;
import android.util.Log;

public class ErrorHandler
{
	private static final String	TAG	= ErrorHandler.class.getSimpleName();
	private static WebClient	webClient;

	// private static ErrorHandler handler;

	public static void init(WebClient webClient)
	{
		ErrorHandler.webClient = webClient;
		// ErrorHandler.handler = new ErrorHandler();
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
					UIUtils.showTip(activity, "Произошла ошибка");
				}

				String trace = (e != null ? Log.getStackTraceString(e) : "(e == null)");
				String exc = (e != null ? e.getLocalizedMessage() : "(e == null)");

				StringBuilder msg = new StringBuilder();
				msg.append("<b>" + TAG + " has detected exception during user runtime</b>\n\n");
				msg.append("<b>Date:</b> " + Utils.now().toGMTString() + "\n");
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
					boolean sent = webClient.sendMail(m.replace("\n", "<br/>"));

					if (activity != null)
					{
						if (sent)
						{
							UIUtils.showTip(activity, "Отчёт с технической информацией отправлен разработчику");
						}
						else
						{
							UIUtils.showTip(activity, "Отправить отчёт не удалось");
						}
					}
				}
				else
				{
					if (activity != null)
					{
						UIUtils.showTip(activity, "Отправить отчёт не удалось (webClient == null)");
					}
				}
			}
			catch (Exception ex)
			{
				UIUtils.showTip(activity, "Во время обработки ошибки произошла ещё одна ошибка :(");
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
