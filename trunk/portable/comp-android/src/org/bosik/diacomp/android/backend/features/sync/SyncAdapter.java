package org.bosik.diacomp.android.backend.features.sync;

import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.android.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.android.backend.features.diary.DiaryWebService;
import org.bosik.diacomp.core.services.diary.DiaryService;
import android.accounts.Account;
import android.content.AbstractThreadedSyncAdapter;
import android.content.ContentProviderClient;
import android.content.ContentResolver;
import android.content.Context;
import android.content.SyncResult;
import android.os.Bundle;

/**
 * Handle the transfer of data between a server and an app, using the Android sync adapter
 * framework.
 */
public class SyncAdapter extends AbstractThreadedSyncAdapter
{
	ContentResolver	mContentResolver;

	/**
	 * Set up the sync adapter
	 */
	public SyncAdapter(Context context, boolean autoInitialize)
	{
		super(context, autoInitialize);
		mContentResolver = context.getContentResolver();
	}

	/**
	 * Set up the sync adapter. This form of the constructor maintains compatibility with Android
	 * 3.0 and later platform versions
	 */
	public SyncAdapter(Context context, boolean autoInitialize, boolean allowParallelSyncs)
	{
		super(context, autoInitialize, allowParallelSyncs);
		mContentResolver = context.getContentResolver();
	}

	@Override
	public void onPerformSync(Account account, Bundle extras, String authority, ContentProviderClient provider,
			SyncResult syncResult)
	{
		final int CONNECTION_TIMEOUT = 12000;

		final ContentResolver contentResolver = getContext().getContentResolver();
		DiaryService localDiary = new DiaryLocalService(contentResolver);

		WebClient webClient = new WebClient(CONNECTION_TIMEOUT);
		webClient.setServer("http://diacomp.org/");
		webClient.setUsername("username");
		webClient.setUsername("password");
		DiaryService webDiary = new DiaryWebService(webClient);

		// SyncService.synchronize_v2(localDiary, webDiary, null);
	}
}
