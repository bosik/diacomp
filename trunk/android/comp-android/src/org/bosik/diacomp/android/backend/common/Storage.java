package org.bosik.diacomp.android.backend.common;

import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.android.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.android.backend.features.diary.DiaryWebService;
import org.bosik.diacomp.android.backend.features.foodbase.FoodBaseLocalService;
import org.bosik.diacomp.android.backend.features.foodbase.FoodBaseWebService;
import org.bosik.diacomp.android.frontend.activities.ActivityPreferences;
import org.bosik.diacomp.android.utils.ErrorHandler;
import org.bosik.diacomp.core.entities.business.Food;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.dishbase.DishBaseService;
import org.bosik.diacomp.core.services.foodbase.FoodBaseService;
import android.content.ContentResolver;
import android.content.Context;
import android.content.SharedPreferences;
import android.util.Log;

/**
 * Stores application DAOs as singletons
 *
 * @author Bosik
 */
public class Storage
{
	// FIXME: don't refer to ActivityPreferences here

	private static final String		TAG					= Storage.class.getSimpleName();

	private static final String		FILENAME_FOODBASE	= "FoodBase.xml";
	private static final String		FILENAME_DISHBASE	= "DishBase.xml";
	private static final int		CONNECTION_TIMEOUT	= 6000;

	// DAO

	public static WebClient			webClient;

	public static DiaryService		localDiary;
	public static DiaryService		webDiary;

	public static FoodBaseService	localFoodBase;
	public static FoodBaseService	webFoodBase;

	public static DishBaseService	localDishBase;
	public static DishBaseService	webDishBase;

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

		// TODO: remove it when tested
		Food smokeTest = new Food();
		smokeTest.setName("Smoker");

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
			// try
			// {
			localFoodBase = new FoodBaseLocalService(resolver);
			// }
			// catch (IOException e)
			// {
			// localFoodBase = null;
			// throw new RuntimeException("Failed to create local food base DAO", e);
			// }
		}
		if (null == localDishBase)
		{
			Log.v(TAG, "Local dish base initialization...");
			// try
			// {
			// localDishBase = new DishBaseLocalService(resolver);
			// }
			// catch (IOException e)
			// {
			// localDishBase = null;
			// throw new RuntimeException("Failed to create local dish base DAO", e);
			// }
		}
		if (null == webFoodBase)
		{
			Log.v(TAG, "Web food base initialization...");

			webFoodBase = new FoodBaseWebService(webClient);
		}
		if (null == webDishBase)
		{
			Log.v(TAG, "Web dish base initialization...");
			// webDishBase = new DishBaseWebService(webClient, new SerializerDishBaseXML());
		}

		ErrorHandler.init(webClient);

		// this applies all preferences
		applyPreference(preferences, null);

		// analyze using
		// RelevantIndexator.indexate(localDiary, localFoodBase, localDishBase);
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
}