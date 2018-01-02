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
package org.bosik.diacomp.android.backend.features.analyze;

import android.content.Context;
import android.util.Log;
import org.bosik.diacomp.android.backend.features.diary.LocalDiary;
import org.bosik.diacomp.core.services.analyze.AnalyzeCore;
import org.bosik.diacomp.core.services.analyze.RateService;
import org.bosik.diacomp.core.services.diary.DiaryService;

public class KoofServiceInternal
{
	private static final String TAG                 = KoofServiceInternal.class.getSimpleName();
	private static final int    ANALYZE_DAYS_PERIOD = 14; // TODO: make it preference
	private static final double ANALYZE_ADAPTATION  = 0.995; // TODO: make it preference

	private static RateService instance;

	public static synchronized RateService getInstance(Context context)
	{
		if (null == instance)
		{
			Log.i(TAG, "Rates service initialization...");
			DiaryService localDiary = LocalDiary.getInstance(context);
			AnalyzeCore analyzeService = AnalyzeCoreInternal.getInstance();
			instance = new RateServiceImpl(context, localDiary, analyzeService, ANALYZE_DAYS_PERIOD, ANALYZE_ADAPTATION);
		}
		return instance;
	}
}