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
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.android.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.android.backend.features.diary.DiaryWebService;
import org.bosik.diacomp.android.backend.features.dishbase.DishBaseLocalService;
import org.bosik.diacomp.android.backend.features.foodbase.FoodBaseLocalService;
import org.bosik.diacomp.android.backend.features.search.TagLocalService;
import org.bosik.diacomp.android.utils.ErrorHandler;
import org.bosik.diacomp.core.services.analyze.AnalyzeCore;
import org.bosik.diacomp.core.services.analyze.AnalyzeCoreImpl;
import org.bosik.diacomp.core.services.analyze.KoofService;
import org.bosik.diacomp.core.services.analyze.KoofServiceImpl;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.search.RelevantIndexator;
import org.bosik.diacomp.core.services.search.TagService;
import org.bosik.merklesync.SyncUtils;
import android.content.ContentResolver;
import android.content.Context;
import android.content.SharedPreferences;
import android.os.AsyncTask;
import android.util.Log;

/**
 * Stores application DAOs as singletons
 */
public class Storage
{
	// FIXME: don't refer to ActivityPreferences here

	static final String				TAG					= Storage.class.getSimpleName();

	private static final int		CONNECTION_TIMEOUT	= 12000;
	private static final int		TIMER_DELAY			= 2 * 1000;						// ms
	private static final int		TIMER_INTERVAL		= 10 * 60 * 1000;					// ms

	private static boolean			timerSettedUp		= false;

	// DAO

	public static WebClient			webClient;

	static DiaryService				localDiary;
	public static DiaryService		webDiary;
	public static FoodBaseService	localFoodBase;
	public static DishBaseService	localDishBase;

	private static AnalyzeCore		analyzeCore;
	public static KoofService		koofService;
	public static TagService		tagService;

	private static int				ANALYZE_DAYS_PERIOD	= 14;								// 20;

	/**
	 * Initializes the storage. Might be called sequentially
	 * 
	 * @param context
	 * @param resolver
	 * @param preferences
	 */
	public static void init(Context context, ContentResolver resolver, SharedPreferences preferences)
	{
		Log.i(TAG, "Storage unit initialization...");

		// DAO's setup

		if (null == webClient)
		{
			Log.i(TAG, "Web client initialization...");
			webClient = new WebClient(CONNECTION_TIMEOUT);
			webClient.setServer(context.getString(R.string.server_url));
		}
		if (null == localDiary)
		{
			Log.i(TAG, "Local diary initialization...");
			localDiary = new DiaryLocalService(resolver);
		}
		if (null == webDiary)
		{
			Log.i(TAG, "Web diary initialization...");
			webDiary = new DiaryWebService(webClient);
		}
		if (null == localFoodBase)
		{
			Log.i(TAG, "Local food base initialization...");
			localFoodBase = new FoodBaseLocalService(resolver);
		}
		if (null == localDishBase)
		{
			Log.i(TAG, "Local dish base initialization...");
			localDishBase = new DishBaseLocalService(resolver);
		}

		if (null == analyzeCore)
		{
			// TODO: hardcoded approximation factor
			analyzeCore = new AnalyzeCoreImpl(40.0);
			// analyzeCore = new HardcodedAnalyzeService();
		}

		if (koofService == null)
		{
			// TODO: hardcoded adaptation
			koofService = new KoofServiceImpl(localDiary, analyzeCore, ANALYZE_DAYS_PERIOD, 0.995);
		}

		if (null == tagService)
		{
			Log.i(TAG, "Local tag service initialization...");
			tagService = new TagLocalService(resolver);
		}

		ErrorHandler.init(webClient);

		// this applies all preferences
		applyPreference(preferences, null);
		setupBackgroundTimer();
	}

	public static void syncDiary(String guid)
	{
		new AsyncTask<String, Void, Void>()
		{
			@Override
			protected Void doInBackground(String... guids)
			{
				try
				{
					SyncUtils.synchronize(localDiary, webDiary, guids[0]);
				}
				catch (Exception e)
				{
					// there is nothing to do with it
					e.printStackTrace();
				}
				return null;
			}
		}.execute(guid);
	}

	private static void setupBackgroundTimer()
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

						relevantIndexation();
						analyzeKoofs();

						/**/time = System.currentTimeMillis() - time;
						/**/Log.d(TAG, "Backgrounds done in " + time + " ms");

						return null;
					}
				}.execute();
			}
		};

		new Timer().scheduleAtFixedRate(task, TIMER_DELAY, TIMER_INTERVAL);
	}

	static void relevantIndexation()
	{
		long time = System.currentTimeMillis();
		RelevantIndexator.indexate(tagService, localDiary, localFoodBase, localDishBase);
		Log.v(TAG, String.format("Relevant indexation done in %d msec", System.currentTimeMillis() - time));
	}

	static void analyzeKoofs()
	{
		long time = System.currentTimeMillis();
		koofService.update();
		Log.v(TAG, String.format("Analyzing done in %d msec", System.currentTimeMillis() - time));
	}

	private static boolean check(String testKey, String baseKey)
	{
		return (testKey == null) || testKey.equals(baseKey);
	}

	/**
	 * Applies changed preference for specified key (if null, applies all settings)
	 * 
	 * @param pref
	 *            Preference unit
	 * @param key
	 *            Change prefernce's key
	 */
	public static void applyPreference(SharedPreferences pref, String key)
	{
		Log.v(TAG, "applyPreferences(): key = '" + key + "'");

		// if (check(key, ActivityPreferences.PREF_ACCOUNT_SERVER_KEY))
		// {
		// webClient.setServer(pref.getString(ActivityPreferences.PREF_ACCOUNT_SERVER_KEY,
		// ActivityPreferences.PREF_ACCOUNT_SERVER_DEFAULT));
		// }
	}

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