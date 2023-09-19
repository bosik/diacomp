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
import java.util.function.Consumer;

public class NotificationService extends Service
{
	private static final String NOTIFICATION_CHANNEL_ID         = "org.bosik.diacomp.notifications.elapsedTime";
	private static final int    NOTIFICATION_ID_ELAPSED_TIME    = 1671918884;
	private static final String ACTION_FORCE_RUN                = "runRightNow";
	private static final int    CACHED_INFO_INVALIDATION_PERIOD = 60; // mins

	private final Timer           timer  = new Timer();
	private       ElapsedTimeInfo cachedInfo;
	private       String          cachedMessage;
	private       int             ticker = 0;

	public static void start(Context context)
	{
		context.startService(new Intent(context, NotificationService.class));
	}

	public static void stop(Context context)
	{
		context.stopService(new Intent(context, NotificationService.class));
	}

	public static void forceRun(Context context)
	{
		context.startService(new Intent(ACTION_FORCE_RUN, null, context, NotificationService.class));
	}

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
				ticker = (ticker + 1) % CACHED_INFO_INVALIDATION_PERIOD;

				if (preferences.getBooleanValue(PreferenceID.ANDROID_SHOW_TIME_AFTER))
				{
					if (cachedInfo == null || ticker == 0)
					{
						runNotificationUpdateAsync();
					}
					else
					{
						updateNotification(cachedInfo);
					}
				}
				else
				{
					hideElapsedTime(NotificationService.this);
				}
			}
		}, 0, Utils.MsecPerMin);
	}

	@Override
	public int onStartCommand(Intent intent, int flags, int startId)
	{
		if (intent != null && ACTION_FORCE_RUN.equals(intent.getAction()))
		{
			ticker = 0;
			runNotificationUpdateAsync();
		}

		return super.onStartCommand(intent, flags, startId);
	}

	public void runNotificationUpdateAsync()
	{
		new UpdateNotificationTask(this, info -> {
			updateNotification(info);
			cachedInfo = info;
		}).execute();
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

	private static void hideElapsedTime(Context context)
	{
		NotificationManagerCompat.from(context).cancel(NOTIFICATION_ID_ELAPSED_TIME);
	}

	public void updateNotification(ElapsedTimeInfo info)
	{
		final String message = buildMessage(info, new Date());

		if (!Utils.isEqual(message, cachedMessage))
		{
			setNotificationMessage(message);
			cachedMessage = message;
		}
	}

	private String buildMessage(ElapsedTimeInfo info, Date now)
	{
		final StringBuilder msg = new StringBuilder();

		if (info.getLastMealTime() != null)
		{
			int timeInSeconds = (int) (now.getTime() - info.getLastMealTime().getTime()) / Utils.MsecPerSec;

			msg.append(Utils.formatTimePeriod(timeInSeconds));
			msg.append(" ");
			msg.append(getString(R.string.notification_elapsed_time_meal));
		}

		if (info.getLastInsTime() != null)
		{
			int timeInSeconds = (int) (now.getTime() - info.getLastInsTime().getTime()) / Utils.MsecPerSec;

			if (msg.length() > 0)
			{
				msg.append(",\n");
			}

			msg.append(Utils.formatTimePeriod(timeInSeconds));
			msg.append(" ");
			msg.append(getString(R.string.notification_elapsed_time_injection));
		}

		return msg.toString();
	}

	private void setNotificationMessage(String message)
	{
		if (!message.isEmpty())
		{
			Intent resultIntent = new Intent(this, ActivityMain.class);
			TaskStackBuilder stackBuilder = TaskStackBuilder.create(this);
			stackBuilder.addParentStack(ActivityMain.class);
			stackBuilder.addNextIntent(resultIntent);
			PendingIntent resultPendingIntent = stackBuilder.getPendingIntent(0, PendingIntent.FLAG_UPDATE_CURRENT);

			Notification notification = new Builder(this, NOTIFICATION_CHANNEL_ID)
					.setSmallIcon(R.drawable.icon)
					.setOngoing(true)
					.setStyle(new NotificationCompat.BigTextStyle().bigText(message))
					.setContentText(message)
					.setPriority(NotificationCompat.PRIORITY_DEFAULT)
					.setOnlyAlertOnce(true)
					.setContentIntent(resultPendingIntent)
					.build();

			NotificationManagerCompat.from(this).notify(NOTIFICATION_ID_ELAPSED_TIME, notification);
			startForeground(NOTIFICATION_ID_ELAPSED_TIME, notification);
		}
		else
		{
			hideElapsedTime(this);
		}
	}
}

class ElapsedTimeInfo
{
	private Date lastMealTime;
	private Date lastInsTime;

	Date getLastMealTime()
	{
		return lastMealTime;
	}

	void setLastMealTime(Date lastMealTime)
	{
		this.lastMealTime = lastMealTime;
	}

	Date getLastInsTime()
	{
		return lastInsTime;
	}

	void setLastInsTime(Date lastInsTime)
	{
		this.lastInsTime = lastInsTime;
	}
}

class UpdateNotificationTask extends AsyncTask<Void, Void, ElapsedTimeInfo>
{
	private final DiaryService              diary;
	private final Consumer<ElapsedTimeInfo> callback;

	UpdateNotificationTask(Context context, Consumer<ElapsedTimeInfo> callback)
	{
		this.diary = LocalDiary.getInstance(context);
		this.callback = callback;
	}

	private Date getLastMealTime(List<Versioned<DiaryRecord>> records)
	{
		MealRecord rec = PostprandUtils.findLastMeal(records);
		return (rec != null)
				? rec.getTime()
				: null;
	}

	private Date getLastInsTime(List<Versioned<DiaryRecord>> records)
	{
		InsRecord rec = PostprandUtils.findLastIns(records);
		return rec != null
				? rec.getTime()
				: null;
	}

	@Override
	protected ElapsedTimeInfo doInBackground(Void... arg0)
	{
		final List<Versioned<DiaryRecord>> records = PostprandUtils.findLastRecordsReversed(
				diary, new Date(), (long) Utils.SecPerDay);

		final ElapsedTimeInfo info = new ElapsedTimeInfo();
		info.setLastMealTime(getLastMealTime(records));
		info.setLastInsTime(getLastInsTime(records));

		return info;
	}

	@Override
	protected void onPostExecute(ElapsedTimeInfo info)
	{
		callback.accept(info);
	}
}