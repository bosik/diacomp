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
package org.bosik.diacomp.android.backend.common;

import java.util.Timer;
import java.util.TimerTask;
import org.bosik.diacomp.android.backend.features.analyze.KoofServiceInternal;
import org.bosik.diacomp.android.backend.features.diary.LocalDiary;
import org.bosik.diacomp.android.backend.features.dishbase.LocalDishBase;
import org.bosik.diacomp.android.backend.features.foodbase.LocalFoodBase;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.search.RelevantIndexator;
import org.bosik.diacomp.core.utils.Utils;
import android.content.ContentResolver;
import android.content.Context;
import android.os.AsyncTask;

public class Storage
{
	static final String			TAG						= Storage.class.getSimpleName();

	private static final long	TIMER_DELAY				= 2 * Utils.MsecPerSec;			// ms
	private static final long	TIMER_INTERVAL_HEAVY	= 10 * Utils.MsecPerMin;		// ms

	private static Timer		timerHeavy;

	private static boolean		timerSettedUp			= false;

	/**
	 * Initializes the storage. Might be called sequentially
	 * 
	 * @param context
	 */
	public static void init(final Context context)
	{
		setupBackgroundTimer(context);
	}

	private static synchronized void setupBackgroundTimer(final Context context)
	{
		if (timerSettedUp)
		{
			return;
		}
		timerSettedUp = true;

		TimerTask taskHeavy = new TimerTask()
		{
			@Override
			public void run()
			{
				new AsyncTask<Void, Void, Void>()
				{
					@Override
					protected Void doInBackground(Void... arg0)
					{
						relevantIndexation(context.getContentResolver());
						analyzeKoofs(context);
						return null;
					}
				}.execute();
			}
		};

		if (timerHeavy != null)
		{
			timerHeavy.cancel();
			timerHeavy.purge();
		}
		timerHeavy = new Timer();
		timerHeavy.scheduleAtFixedRate(taskHeavy, TIMER_DELAY, TIMER_INTERVAL_HEAVY);

	}

	static void relevantIndexation(ContentResolver resolver)
	{
		final DiaryService diary = LocalDiary.getInstance(resolver);
		final FoodBaseService foodBase = LocalFoodBase.getInstance(resolver);
		final DishBaseService dishBase = LocalDishBase.getInstance(resolver);

		RelevantIndexator.indexate(diary, foodBase, dishBase);
	}

	static void analyzeKoofs(Context context)
	{
		KoofServiceInternal.getInstance(context).update();
	}
}