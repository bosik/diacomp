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

import android.accounts.Account;
import android.accounts.AccountManager;
import android.content.AbstractThreadedSyncAdapter;
import android.content.ContentProviderClient;
import android.content.Context;
import android.content.SyncResult;
import android.os.Bundle;
import android.util.Log;

import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.android.backend.common.webclient.WebClientInternal;
import org.bosik.diacomp.android.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.android.backend.features.diary.DiaryWebService;
import org.bosik.diacomp.android.backend.features.dishbase.DishBaseLocalService;
import org.bosik.diacomp.android.backend.features.dishbase.DishBaseWebService;
import org.bosik.diacomp.android.backend.features.foodbase.FoodBaseLocalService;
import org.bosik.diacomp.android.backend.features.foodbase.FoodBaseWebService;
import org.bosik.diacomp.android.backend.features.preferences.account.PreferencesLocalService;
import org.bosik.diacomp.android.backend.features.preferences.account.PreferencesWebService;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.preferences.PreferencesService;
import org.bosik.diacomp.core.services.preferences.PreferencesSync;
import org.bosik.merklesync.SyncUtils.Synchronizer2;

public class SyncAdapter extends AbstractThreadedSyncAdapter
{
	private static final String TAG = SyncAdapter.class.getSimpleName();

	// Average diary entry size: 177.8 bytes
	// Average food entry size: 128.6 bytes
	// Average dish entry size: 466.1 bytes
	// (2018-01-03)

	// Tuned to transfer 32 Kb per block
	private static final int MAX_DIARY_READ  = 184;
	private static final int MAX_DIARY_WRITE = 184;
	private static final int MAX_FOOD_READ   = 255;
	private static final int MAX_FOOD_WRITE  = 255;
	private static final int MAX_DISH_READ   = 70;
	private static final int MAX_DISH_WRITE  = 70;

	private final AccountManager mAccountManager;

	public SyncAdapter(Context context, boolean autoInitialize)
	{
		super(context, autoInitialize);
		mAccountManager = AccountManager.get(context);
	}

	public SyncAdapter(Context context, boolean autoInitialize, boolean allowParallelSyncs)
	{
		super(context, autoInitialize, allowParallelSyncs);
		mAccountManager = AccountManager.get(context);
	}

	@Override
	public void onPerformSync(Account account, Bundle extras, String authority, ContentProviderClient provider, SyncResult syncResult)
	{
		try
		{
			syncResult.stats.numIoExceptions = 0;
			final String username = account.name;
			final String password = mAccountManager.getPassword(account);
			final WebClient webClient = WebClientInternal.getInstance(getContext(), username, password);

			syncDiary(webClient, syncResult);
			syncFood(webClient, syncResult);
			syncDish(webClient, syncResult);
			syncPreferences(webClient, syncResult);
		}
		catch (Exception e)
		{
			Log.e(TAG, "Sync failed: common", e);
			syncResult.stats.numIoExceptions++;
		}
	}

	private void syncDiary(WebClient webClient, SyncResult syncResult)
	{
		try
		{
			final DiaryService localDiary = new DiaryLocalService(getContext());
			final DiaryService webDiary = new DiaryWebService(webClient);
			final int counterDiary = new Synchronizer2<>(localDiary, webDiary, MAX_DIARY_READ, MAX_DIARY_WRITE).synchronize();
			Log.v(TAG, "Synchronized diary entries: " + counterDiary);
		}
		catch (Exception e)
		{
			Log.e(TAG, "Sync failed: diary", e);
			syncResult.stats.numIoExceptions++;
		}
	}

	private void syncFood(WebClient webClient, SyncResult syncResult)
	{
		try
		{
			final FoodBaseService localFoodBase = FoodBaseLocalService.getInstance(getContext());
			final FoodBaseService webFoodBase = new FoodBaseWebService(webClient);
			final int counterFood = new Synchronizer2<>(localFoodBase, webFoodBase, MAX_FOOD_READ, MAX_FOOD_WRITE).synchronize();
			Log.v(TAG, "Synchronized food entries: " + counterFood);
		}
		catch (Exception e)
		{
			Log.e(TAG, "Sync failed: food", e);
			syncResult.stats.numIoExceptions++;
		}
	}

	private void syncDish(WebClient webClient, SyncResult syncResult)
	{
		try
		{
			final DishBaseService localDishBase = DishBaseLocalService.getInstance(getContext());
			final DishBaseService webDishBase = new DishBaseWebService(webClient);
			final int counterDish = new Synchronizer2<>(localDishBase, webDishBase, MAX_DISH_READ, MAX_DISH_WRITE).synchronize();
			Log.v(TAG, "Synchronized dish entries: " + counterDish);
		}
		catch (Exception e)
		{
			Log.e(TAG, "Sync failed: dish", e);
			syncResult.stats.numIoExceptions++;
		}
	}

	private void syncPreferences(WebClient webClient, SyncResult syncResult)
	{
		try
		{
			final PreferencesService localPreferences = new PreferencesLocalService(getContext());
			final PreferencesService webPreferences = new PreferencesWebService(webClient);
			final int counterPreferences = PreferencesSync.synchronizePreferences(localPreferences, webPreferences) ? 1 : 0;
			Log.v(TAG, "Synchronized preference entries: " + counterPreferences);
		}
		catch (Exception e)
		{
			Log.e(TAG, "Sync failed: preferences", e);
			syncResult.stats.numIoExceptions++;
		}
	}
}
