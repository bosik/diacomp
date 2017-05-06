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
import org.bosik.diacomp.core.utils.ZipUtils;
import org.bosik.diacomp.core.utils.ZipUtils.Entry;
import android.content.Context;

public class ImportService
{
	// PART OF PUBLIC API
	public static final String	ENTRY_DIARY			= "diary.json";
	public static final String	ENTRY_FOODBASE		= "foodbase.json";
	public static final String	ENTRY_DISHBASE		= "dishbase.json";
	public static final String	ENTRY_PREFERENCES	= "preferences.json";

	public static void importData(Context context)
	{
		try
		{
			// building services

			WebClient client = WebClientInternal.getInstance(context);
			DiaryLocalService diaryService = new DiaryLocalService(context);
			FoodBaseLocalService foodbaseService = new FoodBaseLocalService(context);
			DishBaseLocalService dishbaseService = new DishBaseLocalService(context.getContentResolver());
			PreferencesLocalService preferencesService = new PreferencesLocalService(context.getContentResolver());

			// download data
			InputStream stream = client.loadStream("api/export");

			try
			{
				List<Entry> entries = ZipUtils.unzip(stream);

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
									diaryService.importData(data);
									break;
								}

								case ENTRY_FOODBASE:
								{
									foodbaseService.importData(data);
									break;
								}

								case ENTRY_DISHBASE:
								{
									dishbaseService.importData(data);
									break;
								}

								case ENTRY_PREFERENCES:
								{
									preferencesService.importData(data);
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
