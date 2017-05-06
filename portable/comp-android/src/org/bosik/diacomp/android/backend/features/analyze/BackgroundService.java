package org.bosik.diacomp.android.backend.features.analyze;

import java.util.Timer;
import java.util.TimerTask;
import org.bosik.diacomp.android.backend.features.diary.LocalDiary;
import org.bosik.diacomp.android.backend.features.dishbase.LocalDishBase;
import org.bosik.diacomp.android.backend.features.foodbase.LocalFoodBase;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.search.RelevantIndexator;
import org.bosik.diacomp.core.utils.Utils;
import android.app.Service;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.IBinder;

public class BackgroundService extends Service
{
	private static final long	TIMER_DELAY		= 2 * Utils.MsecPerSec;	// ms
	private static final long	TIMER_INTERVAL	= 10 * Utils.MsecPerMin;

	private Timer				timer			= new Timer();
	// SharedPreferences preferences;

	@Override
	public IBinder onBind(Intent intent)
	{
		return null;
	}

	@Override
	public void onCreate()
	{
		super.onCreate();
		// preferences = PreferenceManager.getDefaultSharedPreferences(this);

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
						relevantIndexation(getContentResolver());
						analyzeKoofs(BackgroundService.this);
						return null;
					}

					void relevantIndexation(ContentResolver resolver)
					{
						final DiaryService diary = LocalDiary.getInstance(BackgroundService.this);
						final FoodBaseService foodBase = LocalFoodBase.getInstance(resolver);
						final DishBaseService dishBase = LocalDishBase.getInstance(resolver);

						RelevantIndexator.indexate(diary, foodBase, dishBase);
					}

					void analyzeKoofs(Context context)
					{
						KoofServiceInternal.getInstance(context).update();
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
