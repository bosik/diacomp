package org.bosik.diacomp.android.backend.features.sync;

import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.android.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.android.backend.features.diary.DiaryWebService;
import org.bosik.diacomp.android.backend.features.dishbase.DishBaseLocalService;
import org.bosik.diacomp.android.backend.features.dishbase.DishBaseWebService;
import org.bosik.diacomp.android.backend.features.foodbase.FoodBaseLocalService;
import org.bosik.diacomp.android.backend.features.foodbase.FoodBaseWebService;
import org.bosik.diacomp.android.backend.features.preferences.PreferencesLocalService;
import org.bosik.diacomp.android.backend.features.preferences.PreferencesWebService;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.preferences.PreferencesService;
import org.bosik.diacomp.core.services.sync.SyncUtils;
import android.accounts.Account;
import android.accounts.AccountManager;
import android.content.AbstractThreadedSyncAdapter;
import android.content.ContentProviderClient;
import android.content.ContentResolver;
import android.content.Context;
import android.content.SyncResult;
import android.os.Bundle;
import android.util.Log;

/**
 * Handle the transfer of data between a server and an app, using the Android sync adapter
 * framework.
 */
public class SyncAdapter extends AbstractThreadedSyncAdapter
{
	private static final String		TAG	= SyncAdapter.class.getSimpleName();

	private final ContentResolver	mContentResolver;
	private final AccountManager	mAccountManager;
	private static WebClient		webClient;

	/**
	 * Set up the sync adapter
	 */
	public SyncAdapter(Context context, boolean autoInitialize)
	{
		super(context, autoInitialize);
		mContentResolver = context.getContentResolver();
		mAccountManager = AccountManager.get(context);
	}

	/**
	 * Set up the sync adapter. This form of the constructor maintains compatibility with Android
	 * 3.0 and later platform versions
	 */
	public SyncAdapter(Context context, boolean autoInitialize, boolean allowParallelSyncs)
	{
		super(context, autoInitialize, allowParallelSyncs);
		mContentResolver = context.getContentResolver();
		mAccountManager = AccountManager.get(context);
	}

	@Override
	public void onPerformSync(Account account, Bundle extras, String authority, ContentProviderClient provider,
			SyncResult syncResult)
	{
		try
		{
			Log.i(TAG, "Sync fired");

			// Preparing local services

			ContentResolver contentResolver = getContext().getContentResolver();
			DiaryService localDiary = new DiaryLocalService(contentResolver);
			FoodBaseService localFoodBase = new FoodBaseLocalService(contentResolver);
			DishBaseService localDishBase = new DishBaseLocalService(contentResolver);
			PreferencesService localPreferences = new PreferencesLocalService(contentResolver);

			// Preparing web services

			final int connectionTimeout = Integer.parseInt(getContext().getString(R.string.server_timeout));
			final String serverURL = getContext().getString(R.string.server_url);
			final String name = account.name;
			final String password = mAccountManager.getPassword(account);

			if (webClient == null)
			{
				webClient = new WebClient(connectionTimeout);
			}

			webClient.setServer(serverURL);
			webClient.setUsername(name);
			webClient.setPassword(password);

			DiaryService webDiary = new DiaryWebService(webClient);
			FoodBaseService webFoodBase = new FoodBaseWebService(webClient);
			DishBaseService webDishBase = new DishBaseWebService(webClient);
			PreferencesService webPreferences = new PreferencesWebService(webClient);

			// Synchronize

			/**/long time = System.currentTimeMillis();
			int counterDiary = SyncUtils.synchronize_v2(localDiary, webDiary, null);
			int counterFood = SyncUtils.synchronize_v2(localFoodBase, webFoodBase, null);
			int counterDish = SyncUtils.synchronize_v2(localDishBase, webDishBase, null);
			int countPreferences = SyncUtils.synchronizePreferences(localPreferences, webPreferences) ? 1 : 0;

			/**/time = System.currentTimeMillis() - time;
			/**/Log.i(TAG, String.format("SPC: synchronized in %d ms, items transferred: %d/%d/%d/%d", time,
					counterDiary, counterFood, counterDish, countPreferences));
		}
		catch (Exception e)
		{
			Log.e(TAG, "Sync failed", e);
			syncResult.stats.numIoExceptions = 1;
		}
	}
}
