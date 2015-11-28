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
import org.bosik.diacomp.android.backend.features.search.TagServiceInternal;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.search.RelevantIndexator;
import org.bosik.diacomp.core.services.search.TagService;
import android.content.ContentResolver;
import android.content.Context;
import android.content.SharedPreferences;
import android.os.AsyncTask;
import android.util.Log;

public class Storage
{
	static final String			TAG				= Storage.class.getSimpleName();

	private static final int	TIMER_DELAY		= 2 * 1000;						// ms
	private static final int	TIMER_INTERVAL	= 10 * 60 * 1000;				// ms

	private static boolean		timerSettedUp	= false;

	/**
	 * Initializes the storage. Might be called sequentially
	 * 
	 * @param context
	 * @param resolver
	 * @param preferences
	 */
	public static void init(Context context, ContentResolver resolver, SharedPreferences preferences)
	{
		// ErrorHandler.init(getWebClient(context));
		setupBackgroundTimer(resolver);
	}

	private static synchronized void setupBackgroundTimer(final ContentResolver resolver)
	{
		if (timerSettedUp)
		{
			return;
		}
		timerSettedUp = true;

		TimerTask task = new TimerTask()
		{
			@Override
			public void run()
			{
				new AsyncTask<Void, Void, Void>()
				{
					@Override
					protected Void doInBackground(Void... arg0)
					{
						/**/long time = System.currentTimeMillis();

						relevantIndexation(resolver);
						analyzeKoofs(resolver);

						/**/Log.d(TAG, "Backgrounds done in " + (System.currentTimeMillis() - time) + " ms");

						return null;
					}
				}.execute();
			}
		};

		new Timer().scheduleAtFixedRate(task, TIMER_DELAY, TIMER_INTERVAL);
	}

	static void relevantIndexation(ContentResolver resolver)
	{
		long time = System.currentTimeMillis();

		final TagService tagService = TagServiceInternal.getInstance();
		final DiaryService diary = LocalDiary.getInstance(resolver);
		final FoodBaseService foodBase = LocalFoodBase.getInstance(resolver);
		final DishBaseService dishBase = LocalDishBase.getInstance(resolver);

		RelevantIndexator.indexate(tagService, diary, foodBase, dishBase);

		Log.v(TAG, String.format("Relevant indexation done in %d msec", System.currentTimeMillis() - time));
	}

	static void analyzeKoofs(ContentResolver resolver)
	{
		long time = System.currentTimeMillis();

		KoofServiceInternal.getInstance(resolver).update();

		Log.v(TAG, String.format("Analyzing done in %d msec", System.currentTimeMillis() - time));
	}

	// public static void syncDiary(String guid)
	// {
	// new AsyncTask<String, Void, Void>()
	// {
	// @Override
	// protected Void doInBackground(String... guids)
	// {
	// try
	// {
	// SyncUtils.synchronize(localDiary, webDiary, guids[0]);
	// }
	// catch (Exception e)
	// {
	// // there is nothing to do with it
	// e.printStackTrace();
	// }
	// return null;
	// }
	// }.execute(guid);
	// }

	// private static void speedTest()
	// {
	// Serializer<Versioned<FoodItem>> serializer = new SerializerFoodItem();
	// List<Versioned<FoodItem>> items = localFoodBase.findAll(true);
	//
	// long time = System.currentTimeMillis();
	// String s = serializer.writeAll(items);
	// Log.e(TAG,
	// String.format("%d items serialized withing %d msec", items.size(), System.currentTimeMillis()
	// - time));
	//
	// time = System.currentTimeMillis();
	// serializer.readAll(s);
	// Log.e(TAG, String.format("%d items de-serialized withing %d msec", items.size(),
	// System.currentTimeMillis()
	// - time));
	// }

	// private static String pair(String name, double value)
	// {
	// return String.format(Locale.US, "%s=\"%.1f\"", name, value).replace(",", ".");
	// }
	//
	// private static String pair(String name, String value)
	// {
	// return String.format("%s=\"%s\"", name, value.replace("\"", "&quot;"));
	// }
	//
	// private static void buildFoodList()
	// {
	// String result = "";
	//
	// List<Versioned<FoodItem>> foods = localFoodBase.findAll(false);
	// for (Versioned<FoodItem> item : foods)
	// {
	// FoodItem food = item.getData();
	// if (food.getName().contains("Теремок"))
	// {
	// result = String.format("\t<food %s %s %s %s %s %s table=\"True\" tag=\"0\"/>",
	// pair("id", Utils.generateGuid().toUpperCase()), pair("name", food.getName()),
	// pair("prots", food.getRelProts()), pair("fats", food.getRelFats()),
	// pair("carbs", food.getRelCarbs()), pair("val", food.getRelValue()));
	// Log.e(TAG, result);
	// }
	// }
	// }
}