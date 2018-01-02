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
import org.bosik.diacomp.android.backend.features.preferences.account.PreferencesLocalService;
import org.bosik.diacomp.core.services.analyze.AnalyzeCore;
import org.bosik.diacomp.core.services.analyze.RateService;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.services.preferences.PreferencesTypedService;

public class KoofServiceInternal
{
	private static final String TAG                 = KoofServiceInternal.class.getSimpleName();
	private static final int    ANALYZE_DAYS_PERIOD = 14; // TODO: make it preference
	private static final double ANALYZE_ADAPTATION  = 0.995; // TODO: make it preference

	private static RateService instanceAuto;
	private static RateService instanceManual;

	public static synchronized RateService getInstanceAuto(Context context)
	{
		if (instanceAuto == null)
		{
			Log.i(TAG, "Rates service (auto) initialization...");
			DiaryService localDiary = LocalDiary.getInstance(context);
			AnalyzeCore analyzeService = AnalyzeCoreInternal.getInstance();
			instanceAuto = new RateServiceAuto(context, localDiary, analyzeService, ANALYZE_DAYS_PERIOD, ANALYZE_ADAPTATION);
		}
		return instanceAuto;
	}

	public static synchronized RateService getInstanceManual(Context context)
	{
		// if (instanceManual == null)
		{
			Log.i(TAG, "Rates service (manual) initialization...");
			instanceManual = new RateServiceManual(context);
		}
		return instanceManual;
	}

	public static synchronized RateService getInstance(Context context)
	{
		PreferencesTypedService preferences = new PreferencesTypedService(new PreferencesLocalService(context));
		boolean autoRates = preferences.getBooleanValue(PreferenceID.RATES_AUTO);

		return autoRates ? getInstanceAuto(context) : getInstanceManual(context);
	}
}