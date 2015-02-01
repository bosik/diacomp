package org.bosik.diacomp.android.backend.common;

import java.util.Timer;
import java.util.TimerTask;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.android.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.android.backend.features.diary.DiaryWebService;
import org.bosik.diacomp.android.backend.features.dishbase.DishBaseLocalService;
import org.bosik.diacomp.android.backend.features.dishbase.DishBaseWebService;
import org.bosik.diacomp.android.backend.features.foodbase.FoodBaseLocalService;
import org.bosik.diacomp.android.backend.features.foodbase.FoodBaseWebService;
import org.bosik.diacomp.android.backend.features.search.TagLocalService;
import org.bosik.diacomp.android.frontend.activities.ActivityPreferences;
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
import org.bosik.diacomp.core.services.sync.SyncService;
import android.content.ContentResolver;
import android.content.Context;
import android.content.SharedPreferences;
import android.os.AsyncTask;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;

/**
 * Stores application DAOs as singletons
 * 
 * @author Bosik
 */
public class Storage
{
	// FIXME: don't refer to ActivityPreferences here

	static final String				TAG					= Storage.class.getSimpleName();

	private static final int		CONNECTION_TIMEOUT	= 12000;

	private static boolean			timerSettedUp		= false;

	// DAO

	public static WebClient			webClient;

	static DiaryService				localDiary;
	public static DiaryService		webDiary;
	public static FoodBaseService	localFoodBase;
	public static FoodBaseService	webFoodBase;
	public static DishBaseService	localDishBase;
	public static DishBaseService	webDishBase;

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
		Log.v(TAG, "Storage unit initialization...");

		// DAO's setup

		if (null == webClient)
		{
			Log.v(TAG, "Web client initialization...");
			webClient = new WebClient(CONNECTION_TIMEOUT);
		}
		if (null == localDiary)
		{
			Log.v(TAG, "Local diary initialization...");
			localDiary = new DiaryLocalService(resolver);
		}
		if (null == webDiary)
		{
			Log.v(TAG, "Web diary initialization...");
			webDiary = new DiaryWebService(webClient);
		}
		if (null == localFoodBase)
		{
			Log.v(TAG, "Local food base initialization...");
			localFoodBase = new FoodBaseLocalService(resolver);
		}
		if (null == localDishBase)
		{
			Log.v(TAG, "Local dish base initialization...");
			localDishBase = new DishBaseLocalService(resolver);
		}
		if (null == webFoodBase)
		{
			Log.v(TAG, "Web food base initialization...");

			webFoodBase = new FoodBaseWebService(webClient);
		}
		if (null == webDishBase)
		{
			Log.v(TAG, "Web dish base initialization...");
			webDishBase = new DishBaseWebService(webClient);
		}

		if (null == analyzeCore)
		{
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
			Log.v(TAG, "Local tag service initialization...");
			tagService = new TagLocalService(resolver);
		}

		ErrorHandler.init(webClient);

		// this applies all preferences
		applyPreference(preferences, null);

		// runBackgrounds();
		setupSyncTimer(10 * 60 * 1000);
	}

	public static void runBackgrounds()
	{
		new AsyncTask<Void, Void, Void>()
		{
			@Override
			protected Void doInBackground(Void... arg0)
			{
				// FIXME

				long time = System.currentTimeMillis();

				syncDiary();
				syncFoodbase();
				syncDishbase();
				relevantIndexation();
				analyzeKoofs();

				time = System.currentTimeMillis() - time;
				Log.d(TAG, "Backgrounds done in " + time + " msec");

				return null;
			}
		}.execute();
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
					SyncService.synchronize(localDiary, webDiary, guids[0]);
				}
				catch (Exception e)
				{
					// there is nothing to do with it
					// e.printStackTrace();
				}
				return null;
			}
		}.execute(guid);
	}

	private static void setupSyncTimer(long interval)
	{
		if (timerSettedUp)
		{
			return;
		}
		timerSettedUp = true;

		TimerTask task = new TimerTask()
		{
			private final Handler	mHandler	= new Handler(Looper.getMainLooper());

			@Override
			public void run()
			{
				mHandler.post(new Runnable()
				{
					@Override
					public void run()
					{
						runBackgrounds();
					}
				});
			}
		};

		Timer timer = new Timer();
		// timer.scheduleAtFixedRate(task, 2000, interval);
		// timer.schedule(task, 2000);
	}

	public static Integer syncDiary()
	{
		try
		{
			Log.v(TAG, "Diary sync...");
			long time = System.currentTimeMillis();
			int syncDiaryItemsCount = SyncService.synchronize_v2(localDiary, webDiary, null);
			Log.v(TAG, String.format("Diary synced in %d msec, total tranferred: %d",
					System.currentTimeMillis() - time, syncDiaryItemsCount));
			return syncDiaryItemsCount;
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return null;
		}
	}

	public static Integer syncFoodbase()
	{
		try
		{
			Log.v(TAG, "Foodbase sync...");
			long time = System.currentTimeMillis();
			int syncFoodItemsCount = SyncService.synchronize_v2(Storage.localFoodBase, Storage.webFoodBase, null);
			Log.v(TAG, String.format("Foodbase synced in %d msec, total tranferred: %d", System.currentTimeMillis()
					- time, syncFoodItemsCount));
			return syncFoodItemsCount;
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return null;
		}
	}

	public static Integer syncDishbase()
	{
		try
		{
			Log.v(TAG, "Dishbase sync...");
			long time = System.currentTimeMillis();
			int syncDishItemsCount = SyncService.synchronize_v2(Storage.localDishBase, Storage.webDishBase, null);
			Log.v(TAG, String.format("Dishbase synced in %d msec, total tranferred: %d", System.currentTimeMillis()
					- time, syncDishItemsCount));
			return syncDishItemsCount;
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return null;
		}
	}

	static void relevantIndexation()
	{
		long time = System.currentTimeMillis();
		RelevantIndexator.indexate(tagService, localDiary, localFoodBase, localDishBase);
		Log.v(TAG, String.format("Relevant indexation done in %d msec", System.currentTimeMillis() - time));
	}

	public static void analyzeKoofs()
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

		if (check(key, ActivityPreferences.PREF_ACCOUNT_SERVER_KEY))
		{
			webClient.setServer(pref.getString(ActivityPreferences.PREF_ACCOUNT_SERVER_KEY,
					ActivityPreferences.PREF_ACCOUNT_SERVER_DEFAULT));
		}

		if (check(key, ActivityPreferences.PREF_ACCOUNT_USERNAME_KEY))
		{
			webClient.setUsername(pref.getString(ActivityPreferences.PREF_ACCOUNT_USERNAME_KEY,
					ActivityPreferences.PREF_ACCOUNT_USERNAME_DEFAULT));
		}

		if (check(key, ActivityPreferences.PREF_ACCOUNT_PASSWORD_KEY))
		{
			webClient.setPassword(pref.getString(ActivityPreferences.PREF_ACCOUNT_PASSWORD_KEY,
					ActivityPreferences.PREF_ACCOUNT_PASSWORD_DEFAULT));
		}
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