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

import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.IBinder;
import org.bosik.diacomp.android.backend.features.diary.LocalDiary;
import org.bosik.diacomp.android.backend.features.dishbase.LocalDishBase;
import org.bosik.diacomp.android.backend.features.foodbase.LocalFoodBase;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.search.RelevantIndexator;
import org.bosik.diacomp.core.utils.Utils;

import java.util.Timer;
import java.util.TimerTask;

public class BackgroundService extends Service
{
	private static final String ACTION_FORCE_RUN = "runRightNow";
	private static final long   TIMER_DELAY      = 2 * Utils.MsecPerSec;    // ms
	private static final long   TIMER_INTERVAL   = 10 * Utils.MsecPerMin;

	private final Timer timer          = new Timer();
	private       long  lastForcedTime = 0L;

	@Override
	public IBinder onBind(Intent intent)
	{
		return null;
	}

	@Override
	public void onCreate()
	{
		super.onCreate();

		timer.scheduleAtFixedRate(new TimerTask()
		{
			@Override
			public void run()
			{
				perfromUpdateAsync();
			}
		}, TIMER_DELAY, TIMER_INTERVAL);
	}

	private void perfromUpdateAsync()
	{
		new AsyncTask<Void, Void, Void>()
		{
			@Override
			protected Void doInBackground(Void... arg0)
			{
				performUpdate(BackgroundService.this);
				return null;
			}

			private void performUpdate(Context context)
			{
				// Relevant indexation
				DiaryService diary = LocalDiary.getInstance(context);
				FoodBaseService foodBase = LocalFoodBase.getInstance(context);
				DishBaseService dishBase = LocalDishBase.getInstance(context);
				RelevantIndexator.indexate(diary, foodBase, dishBase);

				// Rates
				RateServiceInternal.getInstanceAuto(context).update();
				RateServiceInternal.getInstanceManual(context).update();
			}
		}.execute();
	}

	public static void forceRun(Context context)
	{
		context.startService(new Intent(ACTION_FORCE_RUN, null, context, BackgroundService.class));
	}

	@Override
	public int onStartCommand(Intent intent, int flags, int startId)
	{
		if (intent != null && ACTION_FORCE_RUN.equals(intent.getAction()) && (System.nanoTime() - lastForcedTime) > TIMER_INTERVAL * 1000000L)
		{
			lastForcedTime = System.nanoTime();
			perfromUpdateAsync();
		}

		return super.onStartCommand(intent, flags, startId);
	}

	@Override
	public void onDestroy()
	{
		timer.cancel();
	}
}
