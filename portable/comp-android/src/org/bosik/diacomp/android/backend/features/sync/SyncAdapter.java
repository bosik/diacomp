package org.bosik.diacomp.android.backend.features.sync;

import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.android.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.android.backend.features.diary.DiaryWebService;
import org.bosik.diacomp.core.services.diary.DiaryService;
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

			final int CONNECTION_TIMEOUT = 30000;

			String name = account.name;
			String password = mAccountManager.getPassword(account);

			final ContentResolver contentResolver = getContext().getContentResolver();
			DiaryService localDiary = new DiaryLocalService(contentResolver);

			if (webClient == null)
			{
				webClient = new WebClient(CONNECTION_TIMEOUT);
			}
			webClient.setServer("http://192.168.0.100:8190/");
			webClient.setUsername(name);
			webClient.setPassword(password);
			DiaryService webDiary = new DiaryWebService(webClient);

			/**/long time = System.currentTimeMillis();
			int counter = SyncUtils.synchronize_v2(localDiary, webDiary, null);
			/**/time = System.currentTimeMillis() - time;
			/**/Log.i(TAG, "SPC: synchronized in " + time + " ms, items transferred: " + counter);

		}
		catch (Exception e)
		{
			Log.e(TAG, "Sync failed", e);
			syncResult.stats.numIoExceptions = 1;
		}
	}
}
