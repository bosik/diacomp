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
package org.bosik.diacomp.android.backend.features.foodbase;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Build;
import android.os.IBinder;
import android.support.v4.app.NotificationCompat;
import android.support.v4.app.NotificationCompat.Builder;
import android.support.v4.app.NotificationManagerCompat;
import android.util.Log;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.android.backend.common.webclient.WebClientInternal;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.ConnectionException;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.services.base.food.FoodCommonService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.atomic.AtomicBoolean;

public class FoodCommonDownloadService extends Service
{
	private static final String TAG                                 = FoodCommonDownloadService.class.getSimpleName();
	private static final String NOTIFICATION_CHANNEL_ID             = "org.bosik.diacomp.notifications.downloads";
	private static final int    NOTIFICATION_ID_FOOD_COMMON_LOADING = 795419308;
	public static final  String SERVICE_CALLBACK_ID                 = "org.bosik.diacomp.android:FoodCommonDownloadService";
	public static final  String KEY_RESULT                          = "result";

	private static final long TIMEOUT_INITIAL = 15 * Utils.MsecPerSec;
	private static final long TIMEOUT_MAX     = 5 * Utils.MsecPerHour;

	public enum Progress
	{
		INITIALIZATION,
		DOWNLOADING,
		MERGING,
		SAVING,
		DONE_OK,
		DONE_FAIL,
		DONE_FAIL_NO_INTERNET
	}

	@Override
	public void onCreate()
	{
		super.onCreate();
		createNotificationChannel();
	}

	private void createNotificationChannel()
	{
		// Create the NotificationChannel, but only on API 26+ because the NotificationChannel class is new
		// and not in the support library
		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
		{
			NotificationChannel channel = new NotificationChannel(
					NOTIFICATION_CHANNEL_ID,
					getString(R.string.notification_download_channel_name),
					NotificationManager.IMPORTANCE_DEFAULT);
			channel.setDescription(getString(R.string.notification_download_channel_description));
			channel.setSound(null, null);

			getSystemService(NotificationManager.class).createNotificationChannel(channel);
		}
	}

	@Override
	public IBinder onBind(Intent intent)
	{
		return null;
	}

	@Override
	public int onStartCommand(Intent intent, int flags, int startId)
	{
		try
		{
			new FoodLoadingTask(this, TIMEOUT_INITIAL, TIMEOUT_MAX).execute();
		}
		catch (Exception e)
		{
			// about impossible
			Log.e(TAG, e.getMessage(), e);
			// TODO: nobody listens to it now
			Intent notification = new Intent(SERVICE_CALLBACK_ID);
			notification.putExtra(KEY_RESULT, Progress.DONE_FAIL);
			sendBroadcast(notification);
		}

		return START_STICKY;
	}

	private class FoodLoadingTask extends AsyncTask<Void, Progress, Progress>
	{
		private final AtomicBoolean inProgress = new AtomicBoolean(false);

		private final Context                context;
		private final long                   retryTimeout;
		private final long                   maxRetryTimeout;
		private final NotificationManager    notificationManager;
		private final boolean                skipExecution;
		private final String                 notificationTitle;
		private final Map<Progress, Message> messages;

		FoodLoadingTask(Context context, long retryTime, long maxRetryTime)
		{
			if (context == null)
			{
				throw new NullPointerException("Context is null");
			}

			this.context = context;
			this.retryTimeout = retryTime;
			this.maxRetryTimeout = maxRetryTime;
			this.notificationManager = (NotificationManager) context.getSystemService(NOTIFICATION_SERVICE);
			this.notificationTitle = context.getString(R.string.notification_download_foodbase);

			messages = new HashMap<>();
			messages.put(Progress.INITIALIZATION,
					new Message(context.getString(R.string.notification_download_foodbase_initialization), 0));
			messages.put(Progress.DOWNLOADING, new Message(context.getString(R.string.notification_download_foodbase_downloading), 5));
			messages.put(Progress.MERGING, new Message(context.getString(R.string.notification_download_foodbase_merging), 35));
			messages.put(Progress.SAVING, new Message(context.getString(R.string.notification_download_foodbase_saving), 45));
			messages.put(Progress.DONE_OK, new Message(context.getString(R.string.notification_download_foodbase_done), 100));
			messages.put(Progress.DONE_FAIL_NO_INTERNET,
					new Message(context.getString(R.string.notification_download_foodbase_fail_no_internet), -1));

			synchronized (inProgress)
			{
				this.skipExecution = inProgress.get();
				inProgress.set(true);
			}
		}

		@Override
		protected void onPreExecute()
		{
			if (skipExecution)
			{
				return;
			}
		}

		/**
		 * @return true - succeed; false - failed, need to re-schedule; null - skipped
		 */
		@Override
		protected Progress doInBackground(Void... voids)
		{
			if (skipExecution)
			{
				return null;
			}

			try
			{
				publishProgress(Progress.INITIALIZATION);

				FoodBaseLocalService localFoodBase = (FoodBaseLocalService) FoodBaseLocalService.getInstance(context);
				WebClient webClient = WebClientInternal.getInstance(context);
				FoodCommonService foodCommonService = new FoodCommonWebService(webClient);

				publishProgress(Progress.DOWNLOADING);
				List<Versioned<FoodItem>> commonFood = foodCommonService.findAll();
				List<Versioned<FoodItem>> newFood = new ArrayList<>();

				int count = localFoodBase.count("");
				if (count == 0)
				{
					newFood = commonFood;
				}
				else
				{
					publishProgress(Progress.MERGING);
					for (Versioned<FoodItem> food : commonFood)
					{
						if (!localFoodBase.recordExists(food.getId()))
						{
							newFood.add(food);
						}
					}
				}

				publishProgress(Progress.SAVING);
				localFoodBase.save(newFood);

				publishProgress(Progress.DONE_OK);
				return Progress.DONE_OK;
			}
			catch (ConnectionException e)
			{
				Log.e(TAG, e.getMessage(), e);
				publishProgress(Progress.DONE_FAIL_NO_INTERNET);
				return Progress.DONE_FAIL_NO_INTERNET;
			}
			catch (Exception e)
			{
				Log.e(TAG, e.getMessage(), e);
				publishProgress(Progress.DONE_FAIL);
				return Progress.DONE_FAIL;
			}
			finally
			{
				inProgress.set(false);
			}
		}

		class Message
		{
			String text;
			int    progress;

			Message(String text, int progress)
			{
				this.text = text;
				this.progress = progress;
			}
		}

		@Override
		protected void onProgressUpdate(Progress... values)
		{
			final Progress progress = values[0];

			if (progress != null)
			{
				showNotification(messages.get(progress));
			}

			// TODO: nobody listens to it now
			Intent notification = new Intent(SERVICE_CALLBACK_ID);
			notification.putExtra(KEY_RESULT, progress);
			context.sendBroadcast(notification);
		}

		@Override
		protected void onPostExecute(Progress result)
		{
			if (skipExecution)
			{
				return;
			}

			if (result != Progress.DONE_FAIL_NO_INTERNET)
			{
				notificationManager.cancel(NOTIFICATION_ID_FOOD_COMMON_LOADING);
			}

			if (result != Progress.DONE_OK)
			{
				// re-schedule
				final long timeout = (retryTimeout * 2 < maxRetryTimeout) ? (retryTimeout * 2) : maxRetryTimeout;

				new Timer().schedule(new TimerTask()
				{
					@Override
					public void run()
					{
						new FoodLoadingTask(context, timeout, maxRetryTimeout).execute();
					}
				}, timeout);
			}
		}

		private void showNotification(Message message)
		{
			Builder builder = new Builder(context, NOTIFICATION_CHANNEL_ID)
					.setSmallIcon(R.drawable.icon)
					.setContentTitle(notificationTitle)
					.setContentText(message.text)
					.setPriority(NotificationCompat.PRIORITY_DEFAULT)
					.setOnlyAlertOnce(true);

			if (message.progress > 0)
			{
				builder.setOngoing(true);
				builder.setProgress(100, message.progress, false);
			}

			final Notification notification = builder.build();

			NotificationManagerCompat.from(context).notify(NOTIFICATION_ID_FOOD_COMMON_LOADING, notification);
			startForeground(NOTIFICATION_ID_FOOD_COMMON_LOADING, notification);
		}
	}
}
