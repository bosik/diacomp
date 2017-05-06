package org.bosik.diacomp.android.backend.features.notifications;

import java.util.Date;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.diary.LocalDiary;
import org.bosik.diacomp.android.backend.features.preferences.device.DevicePreferences;
import org.bosik.diacomp.android.frontend.activities.ActivityMain;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.diary.PostprandUtils;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.AsyncTask;
import android.os.IBinder;
import android.preference.PreferenceManager;
import android.support.v4.app.NotificationCompat;
import android.support.v4.app.NotificationCompat.Builder;
import android.support.v4.app.TaskStackBuilder;

public class NotificationService extends Service
{
	private static final int	NOTIFICATION_ID_TIME_AFTER	= 1;

	NotificationManager			notificationManager;
	SharedPreferences			preferences;
	private Timer				timer						= new Timer();

	@Override
	public IBinder onBind(Intent intent)
	{
		return null;
	}

	@Override
	public void onCreate()
	{
		super.onCreate();
		preferences = PreferenceManager.getDefaultSharedPreferences(this);
		notificationManager = (NotificationManager) getSystemService(NOTIFICATION_SERVICE);

		timer.scheduleAtFixedRate(new TimerTask()
		{
			@Override
			public void run()
			{
				if (preferences.getBoolean(DevicePreferences.KEY_SHOW_TIME_AFTER, true))
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

	void showElapsedTime()
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
					info += Utils.formatTimePeriod(timeAfterMeal) + " "
							+ getString(R.string.notification_time_after_meal);
				}

				Integer timeAfterIns = getTimeAfterIns(records, now);
				if (timeAfterIns != null)
				{
					info += (info.isEmpty() ? "" : ",\n") + Utils.formatTimePeriod(timeAfterIns) + " "
							+ getString(R.string.notification_time_after_injection);
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
					PendingIntent resultPendingIntent = stackBuilder.getPendingIntent(0,
							PendingIntent.FLAG_UPDATE_CURRENT);
					mBuilder.setContentIntent(resultPendingIntent);
					notificationManager.notify(NOTIFICATION_ID_TIME_AFTER, mBuilder.build());
				}
				else
				{
					hideTimeAfter();
				}
			};
		}.execute();
	}

	void hideTimeAfter()
	{
		notificationManager.cancel(NOTIFICATION_ID_TIME_AFTER);
	}
}
