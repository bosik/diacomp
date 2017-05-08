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
package org.bosik.diacomp.android.backend.features.quickImport;

import org.bosik.diacomp.android.backend.features.quickImport.ImportHelper.Progress;
import org.bosik.diacomp.android.backend.features.quickImport.ImportHelper.ProgressCallback;
import android.app.IntentService;
import android.content.Intent;
import android.util.Log;

public class ImportService extends IntentService
{
	private static final String	TAG					= ImportService.class.getSimpleName();
	public static final String	SERVICE_CALLBACK_ID	= "org.bosik.diacomp.android:ImportService";

	public static final String	KEY_RESULT			= "result";

	// NotificationManager notificationManager;

	public ImportService()
	{
		super("Import service");
	}

	@Override
	protected void onHandleIntent(Intent intent)
	{
		try
		{
			ImportHelper.importData(ImportService.this, new ProgressCallback()
			{
				@SuppressWarnings("synthetic-access")
				@Override
				public void onProgress(Progress step, int done, int total)
				{
					publishResults(step);
				}
			});

			publishResults(Progress.DONE_OK);
		}
		catch (Exception e)
		{
			Log.e(TAG, e.getMessage(), e);
			publishResults(Progress.DONE_FAIL);
		}
	}

	private void publishResults(Progress step)
	{
		Intent intent = new Intent(SERVICE_CALLBACK_ID);
		intent.putExtra(KEY_RESULT, step);
		sendBroadcast(intent);
	}
}
