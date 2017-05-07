package org.bosik.diacomp.android.backend.features.quickImport;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.android.backend.common.webclient.WebClientInternal;
import org.bosik.diacomp.android.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.android.backend.features.dishbase.DishBaseLocalService;
import org.bosik.diacomp.android.backend.features.foodbase.FoodBaseLocalService;
import org.bosik.diacomp.android.backend.features.preferences.account.PreferencesLocalService;
import org.bosik.diacomp.core.utils.Profiler;
import org.bosik.diacomp.core.utils.ZipUtils;
import org.bosik.diacomp.core.utils.ZipUtils.Entry;
import android.content.Context;
import android.util.Log;

public class ImportService
{
	private static final String	TAG					= ImportService.class.getSimpleName();

	// PART OF PUBLIC API
	public static final String	ENTRY_DIARY			= "diary.json";
	public static final String	ENTRY_FOODBASE		= "foodbase.json";
	public static final String	ENTRY_DISHBASE		= "dishbase.json";
	public static final String	ENTRY_PREFERENCES	= "preferences.json";

	public static enum Progress
	{
		INITIALIZATION, LOADING, UNZIPPING, INSTALL_DIARY, INSTALL_FOODBASE, INSTALL_DISHBASE, INSTALL_PREFERENCES;
	}

	public interface ProgressCallback
	{
		void onProgress(Progress step, int done, int total);
	}

	private static void progress(ProgressCallback callback, Progress step)
	{
		if (callback != null)
		{
			callback.onProgress(step, 0, 0);
		}
	}

	private static void progress(ProgressCallback callback, Progress step, int done, int total)
	{
		if (callback != null)
		{
			callback.onProgress(step, done, total);
		}
	}

	public static void importData(Context context)
	{
		importData(context, null);
	}

	public static void importData(Context context, ProgressCallback callback)
	{
		try
		{
			// building services

			Profiler p = new Profiler();

			progress(callback, Progress.INITIALIZATION);
			WebClient client = WebClientInternal.getInstance(context);
			DiaryLocalService diaryService = new DiaryLocalService(context);
			FoodBaseLocalService foodbaseService = new FoodBaseLocalService(context);
			DishBaseLocalService dishbaseService = new DishBaseLocalService(context);
			PreferencesLocalService preferencesService = new PreferencesLocalService(context);

			// download data
			progress(callback, Progress.LOADING);
			InputStream stream = client.loadStream("api/export");
			Log.i(TAG, "Loaded in " + (p.sinceLastCheck() / 1000000) + " ms");

			try
			{
				progress(callback, Progress.UNZIPPING);
				List<Entry> entries = ZipUtils.unzip(stream);
				Log.i(TAG, "Unzipped in " + (p.sinceLastCheck() / 1000000) + " ms");

				// install

				for (Entry entry : entries)
				{
					if (entry.getName() != null)
					{
						InputStream data = new ByteArrayInputStream(entry.getContent());

						try
						{
							switch (entry.getName())
							{
								case ENTRY_DIARY:
								{
									progress(callback, Progress.INSTALL_DIARY);
									diaryService.importData(data);
									Log.i(TAG, "Diary installed in " + (p.sinceLastCheck() / 1000000) + " ms");
									break;
								}

								case ENTRY_FOODBASE:
								{
									progress(callback, Progress.INSTALL_FOODBASE);
									foodbaseService.importData(data);
									Log.i(TAG, "Foodbase installed in " + (p.sinceLastCheck() / 1000000) + " ms");
									break;
								}

								case ENTRY_DISHBASE:
								{
									progress(callback, Progress.INSTALL_DISHBASE);
									dishbaseService.importData(data);
									Log.i(TAG, "Dishbase installed in " + (p.sinceLastCheck() / 1000000) + " ms");
									break;
								}

								case ENTRY_PREFERENCES:
								{
									progress(callback, Progress.INSTALL_PREFERENCES);
									preferencesService.importData(data);
									Log.i(TAG, "Preferences installed in " + (p.sinceLastCheck() / 1000000) + " ms");
									break;
								}
							}
						}
						finally
						{
							data.close();
						}
					}
				}
			}
			finally
			{
				stream.close();
			}
		}
		catch (IOException e)
		{
			throw new RuntimeException(e);
		}
	}
}
