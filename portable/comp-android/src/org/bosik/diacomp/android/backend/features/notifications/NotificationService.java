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

import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.IBinder;
import android.support.v4.app.NotificationCompat;
import android.support.v4.app.NotificationCompat.Builder;
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
	private static final int NOTIFICATION_ID_TIME_AFTER = 1;

	private NotificationManager notificationManager;
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

		final PreferencesTypedService preferences = new PreferencesTypedService(new PreferencesLocalService(this));
		notificationManager = (NotificationManager) getSystemService(NOTIFICATION_SERVICE);

		timer.scheduleAtFixedRate(new TimerTask()
		{
			@Override
			public void run()
			{
				if (preferences.getBooleanValue(PreferenceID.ANDROID_SHOW_TIME_AFTER))
				{
					showElapsedTime();
				}
				else
				{
					hideTimeAfter();
				}
			}
		}, 0, Utils.MsecPerMin);
	}

	@Override
	public void onDestroy()
	{
		hideTimeAfter();
		timer.cancel();
	}

	private void showElapsedTime()
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
				String info = "";

				final Date now = new Date();
				long scanPeriod = Utils.SecPerDay;
				final DiaryService diary = LocalDiary.getInstance(NotificationService.this);
				List<Versioned<DiaryRecord>> records = PostprandUtils.findLastRecordsReversed(diary, now, scanPeriod);

				Integer timeAfterMeal = getTimeAfterMeal(records, now);
				if (timeAfterMeal != null)
				{
					info += Utils.formatTimePeriod(timeAfterMeal) + " " + getString(R.string.notification_time_after_meal);
				}

				Integer timeAfterIns = getTimeAfterIns(records, now);
				if (timeAfterIns != null)
				{
					info += (info.isEmpty() ? "" : ",\n") + Utils.formatTimePeriod(timeAfterIns) + " " + getString(
							R.string.notification_time_after_injection);
				}

				return info;
			}

			@Override
			protected void onPostExecute(String info)
			{
				if (!info.isEmpty())
				{
					Builder mBuilder = new NotificationCompat.Builder(NotificationService.this);
					mBuilder.setContentTitle(getString(R.string.app_name));
					mBuilder.setSmallIcon(R.drawable.icon);
					mBuilder.setOngoing(true);
					mBuilder.setStyle(new NotificationCompat.BigTextStyle().bigText(info));
					mBuilder.setContentText(info);

					Intent resultIntent = new Intent(NotificationService.this, ActivityMain.class);
					TaskStackBuilder stackBuilder = TaskStackBuilder.create(NotificationService.this);
					stackBuilder.addParentStack(ActivityMain.class);
					stackBuilder.addNextIntent(resultIntent);
					PendingIntent resultPendingIntent = stackBuilder.getPendingIntent(0, PendingIntent.FLAG_UPDATE_CURRENT);
					mBuilder.setContentIntent(resultPendingIntent);
					notificationManager.notify(NOTIFICATION_ID_TIME_AFTER, mBuilder.build());
				}
				else
				{
					hideTimeAfter();
				}
			}
		}.execute();
	}

	private void hideTimeAfter()
	{
		notificationManager.cancel(NOTIFICATION_ID_TIME_AFTER);
	}
}
