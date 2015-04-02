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
import org.bosik.diacomp.core.services.preferences.PreferencesSync;
import org.bosik.merklesync.SyncUtils;
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
	private static final String		TAG		= SyncAdapter.class.getSimpleName();

	private final ContentResolver	mContentResolver;
	private final AccountManager	mAccountManager;
	private static WebClient		webClientSingleton;
	private static Object			lock	= new Object();

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

	private WebClient getWebClient(final String username, final String password)
	{
		synchronized (lock)
		{
			final String serverURL = getContext().getString(R.string.server_url);

			if (webClientSingleton == null)
			{
				final int connectionTimeout = Integer.parseInt(getContext().getString(R.string.server_timeout));
				webClientSingleton = new WebClient(connectionTimeout);
			}

			webClientSingleton.setServer(serverURL);
			webClientSingleton.setUsername(username);
			webClientSingleton.setPassword(password);

			return webClientSingleton;
		}
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

			final String username = account.name;
			final String password = mAccountManager.getPassword(account);
			WebClient webClient = getWebClient(username, password);

			DiaryService webDiary = new DiaryWebService(webClient);
			FoodBaseService webFoodBase = new FoodBaseWebService(webClient);
			DishBaseService webDishBase = new DishBaseWebService(webClient);
			PreferencesService webPreferences = new PreferencesWebService(webClient);

			// Synchronize

			/**/long time = System.currentTimeMillis();
			int counterDiary = SyncUtils.synchronize_v2(localDiary, webDiary, null);
			int counterFood = SyncUtils.synchronize_v2(localFoodBase, webFoodBase, null);
			int counterDish = SyncUtils.synchronize_v2(localDishBase, webDishBase, null);
			int countPreferences = PreferencesSync.synchronizePreferences(localPreferences, webPreferences) ? 1 : 0;

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
