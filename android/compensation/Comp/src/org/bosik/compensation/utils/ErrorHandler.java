package org.bosik.compensation.utils;

import org.bosik.compensation.face.BuildConfig;
import org.bosik.compensation.face.UIUtils;
import org.bosik.compensation.persistence.repository.providers.WebClient;
import android.app.Activity;
import android.util.Log;

public class ErrorHandler
{
	private static final String TAG = ErrorHandler.class.getSimpleName();
	private static WebClient webClient;

	// private static ErrorHandler handler;

	public static void init(WebClient webClient)
	{
		ErrorHandler.webClient = webClient;
		// ErrorHandler.handler = new ErrorHandler();
	}

	public static void handle(Throwable e, Activity activity)
	{
		if (BuildConfig.DEBUG)
		{
			Log.e(TAG, "Error handler [mode: debug]: " + e.getMessage());
			throw new RuntimeException(e);
		} else
		{
			Log.e(TAG, "Error handler [mode: release]: " + e.getMessage());

			try
			{
				String trace = Log.getStackTraceString(e);

				StringBuilder msg = new StringBuilder();
				msg.append("<b>" + TAG + " has detected the exception during user runtime.</b>\n\n");
				msg.append("<b>Date:</b> " + Utils.now().toGMTString() + "\n");
				msg.append("<b>User:</b> " + webClient.getUsername() + "\n");
				msg.append("<b>Exception:</b> <font name=\"Courier New\">" + e.getMessage() + "</font>\n\n");
				msg.append(trace);

				String m = msg.toString();
				Log.e(TAG, m);

				boolean sent = webClient.sendMail(m.replace("\n", "<br/>"));

				if ((activity != null) && sent)
				{
					UIUtils.showTip(activity,
							"Произошла ошибка. Отчёт с технической информацией отправлен разработчику.");
				}
			} catch (Exception ex)
			{
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
