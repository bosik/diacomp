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
	private static final long TIMER_DELAY    = 2 * Utils.MsecPerSec;    // ms
	private static final long TIMER_INTERVAL = 10 * Utils.MsecPerMin;

	private final Timer timer = new Timer();

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
				new AsyncTask<Void, Void, Void>()
				{
					@Override
					protected Void doInBackground(Void... arg0)
					{
						relevantIndexation();
						analyzeKoofs(BackgroundService.this);
						return null;
					}

					void relevantIndexation()
					{
						final DiaryService diary = LocalDiary.getInstance(BackgroundService.this);
						final FoodBaseService foodBase = LocalFoodBase.getInstance(BackgroundService.this);
						final DishBaseService dishBase = LocalDishBase.getInstance(BackgroundService.this);

						RelevantIndexator.indexate(diary, foodBase, dishBase);
					}

					void analyzeKoofs(Context context)
					{
						KoofServiceInternal.getInstanceAuto(context).update();
						KoofServiceInternal.getInstanceManual(context).update();
					}
				}.execute();
			}
		}, TIMER_DELAY, TIMER_INTERVAL);
	}

	@Override
	public void onDestroy()
	{
		timer.cancel();
	}
}
