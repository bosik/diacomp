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
import android.content.ContentResolver;
import android.content.Context;
import android.content.SyncResult;
import android.net.Uri;
import android.os.Bundle;
import android.util.Log;
import org.bosik.diacomp.android.backend.common.AccountUtils;
import org.bosik.diacomp.android.backend.common.DiaryContentProvider;
import org.bosik.diacomp.android.backend.common.db.Table;
import org.bosik.diacomp.android.backend.common.db.tables.TableDiary;
import org.bosik.diacomp.android.backend.common.db.tables.TableDishbase;
import org.bosik.diacomp.android.backend.common.db.tables.TableFoodbase;
import org.bosik.diacomp.android.backend.common.db.tables.TablePreferences;
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
import org.bosik.diacomp.core.services.preferences.PreferencesSync;
import org.bosik.merklesync.SyncUtils;

public class SyncAdapter extends AbstractThreadedSyncAdapter
{
	private static final String TAG         = SyncAdapter.class.getSimpleName();
	private static final String CONTENT_KEY = "content.key";

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

	public static void requestSync(Context context, Uri uri)
	{
		if (uri != null)
		{
			final Account account = AccountUtils.getAccount(context);

			final Bundle bundle = new Bundle();
			bundle.putBoolean(ContentResolver.SYNC_EXTRAS_MANUAL, true);
			bundle.putBoolean(ContentResolver.SYNC_EXTRAS_EXPEDITED, true);
			bundle.putString(CONTENT_KEY, uri.toString());

			ContentResolver.requestSync(account, DiaryContentProvider.AUTHORITY, bundle);
		}
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

			final String contentKey = extras.getString(CONTENT_KEY);
			if (contentKey != null)
			{
				final Uri contentUri = Uri.parse(contentKey);
				final Table table = DiaryContentProvider.getTable(contentUri);

				if (table != null)
				{
					final String id = contentUri.getLastPathSegment();

					switch (table.getName())
					{
						case TableDiary.TABLE_NAME:
						{
							syncDiarySingle(webClient, syncResult, id);
							break;
						}

						case TableFoodbase.TABLE_NAME:
						{
							syncFoodSingle(webClient, syncResult, id);
							break;
						}

						case TableDishbase.TABLE_NAME:
						{
							syncDishSingle(webClient, syncResult, id);
							break;
						}

						case TablePreferences.TABLE_NAME:
						{
							syncPreferences(webClient, syncResult);
							break;
						}

						default:
						{
							Log.w(TAG, "Unrecognized table name: " + table.getName());
							break;
						}
					}
				}
				else
				{
					Log.w(TAG, "Unrecognized URI: " + contentUri);
				}
			}
			else
			{
				syncDiary(webClient, syncResult);
				syncFood(webClient, syncResult);
				syncDish(webClient, syncResult);
				syncPreferences(webClient, syncResult);
			}
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
			final int counter = SyncUtils.synchronizeByHashChildren(
					new DiaryLocalService(getContext()),
					new DiaryWebService(webClient),
					MAX_DIARY_READ,
					MAX_DIARY_WRITE
			);
			Log.v(TAG, "Synchronized diary entries: " + counter);
		}
		catch (Exception e)
		{
			Log.e(TAG, "Sync failed: diary", e);
			syncResult.stats.numIoExceptions++;
		}
	}

	private void syncDiarySingle(WebClient webClient, SyncResult syncResult, String id)
	{
		try
		{
			final int counter = SyncUtils.transferSingle(
					new DiaryLocalService(getContext()),
					new DiaryWebService(webClient),
					id
			);
			Log.v(TAG, "Synchronized diary entries: " + counter);
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
			final int counter = SyncUtils.synchronizeByHashChildren(
					FoodBaseLocalService.getInstance(getContext()),
					new FoodBaseWebService(webClient),
					MAX_FOOD_READ,
					MAX_FOOD_WRITE
			);
			Log.v(TAG, "Synchronized food entries: " + counter);
		}
		catch (Exception e)
		{
			Log.e(TAG, "Sync failed: food", e);
			syncResult.stats.numIoExceptions++;
		}
	}

	private void syncFoodSingle(WebClient webClient, SyncResult syncResult, String id)
	{
		try
		{
			final int counter = SyncUtils.transferSingle(
					FoodBaseLocalService.getInstance(getContext()),
					new FoodBaseWebService(webClient),
					id
			);
			Log.v(TAG, "Synchronized food entries: " + counter);
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
			final int counter = SyncUtils.synchronizeByHashChildren(
					DishBaseLocalService.getInstance(getContext()),
					new DishBaseWebService(webClient),
					MAX_DISH_READ,
					MAX_DISH_WRITE
			);
			Log.v(TAG, "Synchronized dish entries: " + counter);
		}
		catch (Exception e)
		{
			Log.e(TAG, "Sync failed: dish", e);
			syncResult.stats.numIoExceptions++;
		}
	}

	private void syncDishSingle(WebClient webClient, SyncResult syncResult, String id)
	{
		try
		{
			final int counter = SyncUtils.transferSingle(
					DishBaseLocalService.getInstance(getContext()),
					new DishBaseWebService(webClient),
					id
			);
			Log.v(TAG, "Synchronized dish entries: " + counter);
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
			final int counter = PreferencesSync.synchronizePreferences(
					new PreferencesLocalService(getContext()),
					new PreferencesWebService(webClient)
			) ? 1 : 0;
			Log.v(TAG, "Synchronized preference entries: " + counter);
		}
		catch (Exception e)
		{
			Log.e(TAG, "Sync failed: preferences", e);
			syncResult.stats.numIoExceptions++;
		}
	}
}
