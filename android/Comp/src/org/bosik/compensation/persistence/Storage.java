package org.bosik.compensation.persistence;

import java.io.IOException;
import java.util.List;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.face.activities.ActivityPreferences;
import org.bosik.compensation.persistence.dao.BaseDAO;
import org.bosik.compensation.persistence.dao.DiaryDAO;
import org.bosik.compensation.persistence.dao.local.LocalDiaryDAO;
import org.bosik.compensation.persistence.dao.local.LocalFoodBaseDAO;
import org.bosik.compensation.persistence.dao.web.WebDiaryDAO;
import org.bosik.compensation.persistence.dao.web.WebFoodBaseDAO;
import org.bosik.compensation.persistence.dao.web.utils.client.WebClient;
import org.bosik.compensation.persistence.serializers.foodbase.FoodBaseXMLSerializer;
import org.bosik.compensation.utils.ErrorHandler;
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
	private static final String		TAG					= Storage.class.getSimpleName();

	private static final String		FILENAME_FOODBASE	= "FoodBase.xml";
	// private static final String FILENAME_DISHBASE = "DishBase.xml";
	private static final int		CONNECTION_TIMEOUT	= 6000;

	// DAO
	public static WebClient			webClient;
	public static DiaryDAO			localDiary;
	public static DiaryDAO			webDiary;
	public static BaseDAO<FoodItem>	localFoodBase;
	public static BaseDAO<FoodItem>	webFoodBase;

	// temp data
	public static List<FoodItem>	foodBase;

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
			localDiary = new LocalDiaryDAO(resolver);
		}
		if (null == webDiary)
		{
			Log.v(TAG, "Web diary initialization...");
			webDiary = new WebDiaryDAO(webClient);
		}
		if (null == localFoodBase)
		{
			Log.v(TAG, "Local foodbase initialization...");
			try
			{
				localFoodBase = new LocalFoodBaseDAO(context, FILENAME_FOODBASE);
				foodBase = localFoodBase.findAll();
			}
			catch (IOException e)
			{
				localFoodBase = null;
				throw new RuntimeException("Failed to create local base DAO", e);
			}
		}
		if (null == Storage.webFoodBase)
		{
			Log.v(TAG, "Web foodbase initialization...");
			webFoodBase = new WebFoodBaseDAO(webClient, new FoodBaseXMLSerializer());
		}

		ErrorHandler.init(webClient);

		// this applies all preferences
		applyPreference(preferences, null);
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