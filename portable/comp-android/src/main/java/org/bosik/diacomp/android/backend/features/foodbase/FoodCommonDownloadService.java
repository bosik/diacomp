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

import android.app.NotificationManager;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.IBinder;
import android.support.v4.app.NotificationCompat;
import android.util.Log;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.android.backend.common.webclient.WebClientInternal;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.services.base.food.FoodCommonService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.atomic.AtomicBoolean;

public class FoodCommonDownloadService extends Service
{
	private static final String TAG                                 = FoodCommonDownloadService.class.getSimpleName();
	private static final int    NOTIFICATION_ID_FOOD_COMMON_LOADING = 9123756;
	public static final  String SERVICE_CALLBACK_ID                 = "org.bosik.diacomp.android:FoodCommonDownloadService";
	public static final  String KEY_RESULT                          = "result";

	private static final long TIMEOUT_INITIAL = 15 * Utils.MsecPerSec;
	private static final long TIMEOUT_MAX     = 5 * Utils.MsecPerHour;

	public enum Progress
	{
		INITIALIZATION,
		LOADING,
		SAVING,
		DONE_OK,
		DONE_FAIL
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

	private static class FoodLoadingTask extends AsyncTask<Void, Progress, Boolean>
	{
		private static final AtomicBoolean inProgress = new AtomicBoolean(false);

		private final Context             context;
		private final long                retryTimeout;
		private final long                maxRetryTimeout;
		private final NotificationManager notificationManager;
		private final boolean             skipExecution;

		public FoodLoadingTask(Context context, long retryTime, long maxRetryTime)
		{
			this.context = Objects.requireNonNull(context);
			this.retryTimeout = retryTime;
			this.maxRetryTimeout = maxRetryTime;
			this.notificationManager = (NotificationManager) context.getSystemService(NOTIFICATION_SERVICE);

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

			NotificationCompat.Builder mBuilder = new NotificationCompat.Builder(context);
			mBuilder.setContentTitle(context.getString(R.string.app_name));
			mBuilder.setSmallIcon(R.drawable.icon);
			mBuilder.setOngoing(true);
			mBuilder.setStyle(new NotificationCompat.BigTextStyle().bigText(context.getString(R.string.notification_loading_foodbase)));
			mBuilder.setContentText(context.getString(R.string.notification_loading_foodbase));

			notificationManager.notify(NOTIFICATION_ID_FOOD_COMMON_LOADING, mBuilder.build());
		}

		/**
		 * @return true - succeed; false - failed, need to re-schedule; null - skipped
		 */
		@Override
		protected Boolean doInBackground(Void... voids)
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

				publishProgress(Progress.LOADING);
				List<Versioned<FoodItem>> commonFood = foodCommonService.findAll();
				List<Versioned<FoodItem>> newFood = new ArrayList<>();

				int count = localFoodBase.count("");
				if (count == 0)
				{
					newFood = commonFood;
				}
				else
				{
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
				return true;
			}
			catch (Exception e)
			{
				Log.e(TAG, e.getMessage(), e);
				publishProgress(Progress.DONE_FAIL);
				return false;
			}
			finally
			{
				inProgress.set(false);
			}
		}

		@Override
		protected void onProgressUpdate(Progress... values)
		{
			final Progress progress = values[0];

			// TODO: nobody listens to it now
			Intent notification = new Intent(SERVICE_CALLBACK_ID);
			notification.putExtra(KEY_RESULT, progress);
			context.sendBroadcast(notification);
		}

		@Override
		protected void onPostExecute(Boolean ok)
		{
			if (skipExecution)
			{
				return;
			}

			notificationManager.cancel(NOTIFICATION_ID_FOOD_COMMON_LOADING);

			if (ok == Boolean.FALSE)
			{
				// re-schedule
				final long timeout = (retryTimeout < maxRetryTimeout) ? (retryTimeout * 2) : retryTimeout;

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
	}
}
