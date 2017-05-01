package org.bosik.diacomp.android.backend.features.quickImport;

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
			// client.setServer("http://192.168.100.5:8190/comp-server/");
			DiaryLocalService diaryService = new DiaryLocalService(context.getContentResolver());
			FoodBaseLocalService foodbaseService = new FoodBaseLocalService(context.getContentResolver());
			DishBaseLocalService dishbaseService = new DishBaseLocalService(context.getContentResolver());
			PreferencesLocalService preferencesService = new PreferencesLocalService(context.getContentResolver());

			// download data

			InputStream stream = client.loadStream("api/export");
			List<Entry> entries = ZipUtils.unzip(stream);

			// install

			for (Entry entry : entries)
			{
				if (entry.getName() != null)
				{
					switch (entry.getName())
					{
						case ENTRY_DIARY:
						{
							diaryService.importData(entry.getContent());
							break;
						}

						case ENTRY_FOODBASE:
						{
							foodbaseService.importData(entry.getContent());
							break;
						}

						case ENTRY_DISHBASE:
						{
							dishbaseService.importData(entry.getContent());
							break;
						}

						case ENTRY_PREFERENCES:
						{
							preferencesService.importData(entry.getContent());
							break;
						}
					}
				}
			}
		}
		catch (IOException e)
		{
			throw new RuntimeException(e);
		}
	}
}
