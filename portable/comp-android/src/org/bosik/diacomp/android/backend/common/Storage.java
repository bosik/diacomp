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
	static final String				TAG					= Storage.class.getSimpleName();

	private static final int		TIMER_DELAY			= 2 * 1000;						// ms
	private static final int		TIMER_INTERVAL		= 10 * 60 * 1000;					// ms

	private static boolean			timerSettedUp		= false;

	// DAO

	private static WebClient		webClient;												// ok

	static DiaryService				localDiary;
	static DiaryService				webDiary;
	private static FoodBaseService	localFoodBase;											// ok
	private static DishBaseService	localDishBase;											// ok

	private static AnalyzeCore		analyzeCore;											// ok
	private static KoofService		koofService;											// ok
	private static TagService		tagService;											// ok

	// TODO: make it preference
	private static int				ANALYZE_DAYS_PERIOD	= 14;

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
		getWebClient(context);

		// TODO: refactor Storage-internal singletone usage (e.g. see relevantIndexation())

		getLocalFoodBase(resolver);
		getLocalDishBase(resolver);

		getLocalDiary(resolver);
		getWebDiary(context);

		getAnalyzeService();
		getKoofService(resolver);

		getTagService();

		ErrorHandler.init(getWebClient(context));

		// this applies all preferences
		applyPreference(preferences, null);
		setupBackgroundTimer(resolver);
	}

	public static synchronized WebClient getWebClient(Context context)
	{
		if (null == webClient)
		{
			Log.i(TAG, "Web client initialization...");
			webClient = WebClient.getInstance(context);
		}
		return webClient;
	}

	public static synchronized DiaryService getWebDiary(Context context)
	{
		if (null == webDiary)
		{
			Log.i(TAG, "Web diary initialization...");
			WebClient webClient = getWebClient(context);
			webDiary = new DiaryWebService(webClient);
		}
		return webDiary;
	}

	public static synchronized DiaryService getLocalDiary(ContentResolver resolver)
	{
		if (null == localDiary)
		{
			Log.i(TAG, "Local diary initialization...");
			localDiary = new DiaryLocalService(resolver);
		}
		return localDiary;
	}

	public static synchronized FoodBaseService getLocalFoodBase(ContentResolver resolver)
	{
		if (null == localFoodBase)
		{
			Log.i(TAG, "Local food base initialization...");
			localFoodBase = new FoodBaseLocalService(resolver);
		}
		return localFoodBase;
	}

	public static synchronized DishBaseService getLocalDishBase(ContentResolver resolver)
	{
		if (null == localDishBase)
		{
			Log.i(TAG, "Local dish base initialization...");
			localDishBase = new DishBaseLocalService(resolver);
		}
		return localDishBase;
	}

	public static synchronized AnalyzeCore getAnalyzeService()
	{
		if (null == analyzeCore)
		{
			// TODO: hardcoded approximation factor
			analyzeCore = new AnalyzeCoreImpl(40.0);
			// analyzeCore = new HardcodedAnalyzeService();
		}
		return analyzeCore;
	}

	public static synchronized KoofService getKoofService(ContentResolver resolver)
	{
		if (koofService == null)
		{
			DiaryService localDiary = getLocalDiary(resolver);
			AnalyzeCore analyzeService = getAnalyzeService();
			// TODO: hardcoded adaptation
			koofService = new KoofServiceImpl(localDiary, analyzeService, ANALYZE_DAYS_PERIOD, 0.995);
		}
		return koofService;
	}

	public static synchronized TagService getTagService()
	{
		if (null == tagService)
		{
			Log.i(TAG, "Local tag service initialization...");
			tagService = new TagLocalService();
		}
		return tagService;
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

	private static void setupBackgroundTimer(final ContentResolver resolver)
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

						/**/time = System.currentTimeMillis() - time;
						/**/Log.d(TAG, "Backgrounds done in " + time + " ms");

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

		final TagService tagService = getTagService();
		final DiaryService diary = getLocalDiary(resolver);
		final FoodBaseService foodBase = getLocalFoodBase(resolver);
		final DishBaseService dishBase = getLocalDishBase(resolver);

		RelevantIndexator.indexate(tagService, diary, foodBase, dishBase);

		Log.v(TAG, String.format("Relevant indexation done in %d msec", System.currentTimeMillis() - time));
	}

	static void analyzeKoofs(ContentResolver resolver)
	{
		long time = System.currentTimeMillis();

		getKoofService(resolver).update();

		Log.v(TAG, String.format("Analyzing done in %d msec", System.currentTimeMillis() - time));
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