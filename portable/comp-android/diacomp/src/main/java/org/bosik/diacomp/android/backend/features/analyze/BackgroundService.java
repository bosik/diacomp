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
package org.bosik.diacomp.android.backend.features.analyze;

import android.app.job.JobInfo;
import android.app.job.JobParameters;
import android.app.job.JobScheduler;
import android.app.job.JobService;
import android.content.ComponentName;
import android.content.Context;
import android.os.AsyncTask;
import android.util.Log;
import org.bosik.diacomp.android.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.android.backend.features.diary.LocalDiary;
import org.bosik.diacomp.android.backend.features.dishbase.DishBaseLocalService;
import org.bosik.diacomp.android.backend.features.foodbase.FoodBaseLocalService;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.android.backend.features.search.UsageIndexDiary;
import org.bosik.diacomp.android.backend.features.search.UsageIndexDishbase;
import org.bosik.diacomp.core.utils.Utils;

public class BackgroundService extends JobService
{
	private static final String TAG = BackgroundService.class.getSimpleName();

	private static final long TIMER_INTERVAL = 60 * Utils.MsecPerMin;
	private static final long TIMER_FLEX     = 5 * Utils.MsecPerMin;
	private static final int  JOB_ID         = 1000;

	public static void start(Context context)
	{
		final ComponentName jobService = new ComponentName(context, BackgroundService.class);
		final JobInfo job = new JobInfo.Builder(JOB_ID, jobService)
				.setPeriodic(TIMER_INTERVAL, TIMER_FLEX)
				// .setRequiresBatteryNotLow(true) // requires API 26+
				.build();
		context.getSystemService(JobScheduler.class)
				.schedule(job);
	}

	public static void forceRun(Context context)
	{
		final ComponentName jobService = new ComponentName(context, BackgroundService.class);
		final JobInfo job = new JobInfo.Builder(JOB_ID, jobService)
				.build();
		context.getSystemService(JobScheduler.class)
				.schedule(job);
	}

	@Override
	public boolean onStartJob(JobParameters params)
	{
		new BackgroundTask().execute(this);
		return true;
	}

	@Override
	public boolean onStopJob(JobParameters params)
	{
		return true; // re-schedule
	}

	private static class BackgroundTask extends AsyncTask<Context, Void, Void>
	{
		@Override
		protected Void doInBackground(Context... args)
		{
			final Context context = args[0];

			// Relevant indexation
			DiaryLocalService diaryService = (DiaryLocalService) LocalDiary.getInstance(context);
			final FoodBaseService foodBaseService = FoodBaseLocalService.getInstance(context);
			final DishBaseService dishBaseService = DishBaseLocalService.getInstance(context);

			Log.d(TAG, "Updating search indexes...");
			UsageIndexDiary.update(diaryService, foodBaseService, dishBaseService, 7);
			UsageIndexDishbase.update(foodBaseService, dishBaseService);

			// Rates
			Log.d(TAG, "Updating insulin rates...");
			RateServiceInternal.getInstanceAuto(context).update();
			RateServiceInternal.getInstanceManual(context).update();
			return null;
		}
	}
}
