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
package org.bosik.diacomp.android.backend.features.notifications;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Build;
import android.os.IBinder;
import android.support.v4.app.NotificationCompat;
import android.support.v4.app.NotificationCompat.Builder;
import android.support.v4.app.NotificationManagerCompat;
import android.support.v4.app.TaskStackBuilder;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.diary.LocalDiary;
import org.bosik.diacomp.android.backend.features.preferences.account.PreferencesLocalService;
import org.bosik.diacomp.android.frontend.activities.ActivityMain;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.diary.PostprandUtils;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.services.preferences.PreferencesTypedService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

import java.util.Date;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

public class NotificationService extends Service
{
	private static final String NOTIFICATION_CHANNEL_ID      = "org.bosik.diacomp.notifications.elapsedTime";
	private static final int    NOTIFICATION_ID_ELAPSED_TIME = 1671918884;

	private Timer timer = new Timer();

	@Override
	public IBinder onBind(Intent intent)
	{
		return null;
	}

	@Override
	public void onCreate()
	{
		super.onCreate();

		createNotificationChannel();
		final PreferencesTypedService preferences = new PreferencesTypedService(new PreferencesLocalService(this));

		timer.scheduleAtFixedRate(new TimerTask()
		{
			@Override
			public void run()
			{
				if (preferences.getBooleanValue(PreferenceID.ANDROID_SHOW_TIME_AFTER))
				{
					showElapsedTime(NotificationService.this);
				}
				else
				{
					hideElapsedTime(NotificationService.this);
				}
			}
		}, 0, Utils.MsecPerMin);
	}

	private void createNotificationChannel()
	{
		// Create the NotificationChannel, but only on API 26+ because the NotificationChannel class is new
		// and not in the support library
		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
		{
			NotificationChannel channel = new NotificationChannel(
					NOTIFICATION_CHANNEL_ID,
					getString(R.string.notification_elapsed_time_channel_name),
					NotificationManager.IMPORTANCE_DEFAULT);
			channel.setDescription(getString(R.string.notification_elapsed_time_channel_description));
			channel.setSound(null, null);

			getSystemService(NotificationManager.class).createNotificationChannel(channel);
		}
	}

	@Override
	public void onDestroy()
	{
		hideElapsedTime(this);
		timer.cancel();
	}

	private static void showElapsedTime(final Context context)
	{
		new AsyncTask<Void, Void, String>()
		{
			/**
			 * @return Time after last meal (in seconds) if found, null otherwise
			 */
			private Integer getTimeAfterMeal(List<Versioned<DiaryRecord>> records, Date now)
			{
				MealRecord rec = PostprandUtils.findLastMeal(records);
				if (rec != null)
				{
					return (int) (now.getTime() - rec.getTime().getTime()) / Utils.MsecPerSec;
				}
				else
				{
					return null;
				}
			}

			/**
			 * @return Time after last ins (in seconds) if found, null otherwise
			 */
			private Integer getTimeAfterIns(List<Versioned<DiaryRecord>> records, Date now)
			{
				InsRecord rec = PostprandUtils.findLastIns(records);
				if (rec != null)
				{
					return (int) (now.getTime() - rec.getTime().getTime()) / Utils.MsecPerSec;
				}
				else
				{
					return null;
				}
			}

			@Override
			protected String doInBackground(Void... arg0)
			{
				final StringBuilder info = new StringBuilder();

				final Date now = new Date();
				final long scanPeriod = Utils.SecPerDay;
				final DiaryService diary = LocalDiary.getInstance(context);
				final List<Versioned<DiaryRecord>> records = PostprandUtils.findLastRecordsReversed(diary, now, scanPeriod);

				final Integer timeAfterMeal = getTimeAfterMeal(records, now);
				if (timeAfterMeal != null)
				{
					info.append(Utils.formatTimePeriod(timeAfterMeal));
					info.append(" ");
					info.append(context.getString(R.string.notification_elapsed_time_meal));
				}

				final Integer timeAfterIns = getTimeAfterIns(records, now);
				if (timeAfterIns != null)
				{
					if (info.length() > 0)
					{
						info.append(",\n");
					}

					info.append(Utils.formatTimePeriod(timeAfterIns));
					info.append(" ");
					info.append(context.getString(R.string.notification_elapsed_time_injection));
				}

				return info.toString();
			}

			@Override
			protected void onPostExecute(String info)
			{
				if (!info.isEmpty())
				{
					Intent resultIntent = new Intent(context, ActivityMain.class);
					TaskStackBuilder stackBuilder = TaskStackBuilder.create(context);
					stackBuilder.addParentStack(ActivityMain.class);
					stackBuilder.addNextIntent(resultIntent);
					PendingIntent resultPendingIntent = stackBuilder.getPendingIntent(0, PendingIntent.FLAG_UPDATE_CURRENT);

					Notification notification = new Builder(context, NOTIFICATION_CHANNEL_ID)
							.setContentTitle(context.getString(R.string.app_name))
							.setSmallIcon(R.drawable.icon)
							.setOngoing(true)
							.setStyle(new NotificationCompat.BigTextStyle().bigText(info))
							.setContentText(info)
							.setPriority(NotificationCompat.PRIORITY_DEFAULT)
							.setOnlyAlertOnce(true)
							.setContentIntent(resultPendingIntent)
							.build();

					NotificationManagerCompat.from(context).notify(NOTIFICATION_ID_ELAPSED_TIME, notification);
				}
				else
				{
					hideElapsedTime(context);
				}
			}
		}.execute();
	}

	private static void hideElapsedTime(Context context)
	{
		NotificationManagerCompat.from(context).cancel(NOTIFICATION_ID_ELAPSED_TIME);
	}
}
