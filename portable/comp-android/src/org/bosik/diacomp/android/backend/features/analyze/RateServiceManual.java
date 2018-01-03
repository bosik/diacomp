/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.android.backend.features.analyze;

import android.content.Context;
import android.util.Log;
import org.bosik.diacomp.android.backend.features.preferences.account.PreferencesLocalService;
import org.bosik.diacomp.core.entities.business.TimedRate;
import org.bosik.diacomp.core.services.analyze.RateService;
import org.bosik.diacomp.core.services.analyze.entities.Koof;
import org.bosik.diacomp.core.services.analyze.entities.KoofList;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.services.preferences.PreferencesTypedService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.core.utils.math.Function;
import org.bosik.diacomp.core.utils.math.Interpolation;
import org.json.JSONException;

import java.util.ArrayList;
import java.util.List;

public class RateServiceManual implements RateService
{
	private static final String TAG = RateServiceManual.class.getSimpleName();

	private Context  context;
	private KoofList coefficients;

	public RateServiceManual(Context context)
	{
		this.context = context;
	}

	@Override
	public void update()
	{
		coefficients = buildCoefficients(loadRates(context));
	}

	@Override
	public Koof getKoof(int time)
	{
		if (coefficients == null)
		{
			update();
		}

		if (coefficients != null)
		{
			return coefficients.getKoof(time);
		}
		else
		{
			return null;
		}
	}

	public static List<TimedRate> loadRates(Context context)
	{
		PreferencesTypedService preferences = new PreferencesTypedService(new PreferencesLocalService(context));
		String data = preferences.getStringValue(PreferenceID.RATES_DATA);

		try
		{
			return TimedRate.readList(data);
		}
		catch (JSONException e)
		{
			Log.e(TAG, "Failed to read rates JSON: " + data, e);
			return new ArrayList<>();
		}
	}

	private static KoofList buildCoefficients(List<TimedRate> timedRates)
	{
		if (timedRates == null || timedRates.isEmpty())
		{
			return null;
		}

		KoofList list = new KoofList();

		for (int i = -1; i < timedRates.size(); i++)
		{
			int iPreStart = i - 1;
			int iStart = i;
			int iEnd = i + 1;
			int iAfterEnd = i + 2;

			int nPreStart = (iPreStart + timedRates.size()) % timedRates.size();
			int nStart = (iStart + timedRates.size()) % timedRates.size();
			int nEnd = iEnd % timedRates.size();
			int nAfterEnd = iAfterEnd % timedRates.size();

			final TimedRate ratePreStart = timedRates.get(nPreStart);
			final TimedRate rateStart = timedRates.get(nStart);
			final TimedRate rateEnd = timedRates.get(nEnd);
			final TimedRate rateAfterEnd = timedRates.get(nAfterEnd);

			double tPreStart = (iPreStart >= 0) ? ratePreStart.getTime() : ratePreStart.getTime() - Utils.MinPerDay;
			double tStart = (iStart >= 0) ? rateStart.getTime() : rateStart.getTime() - Utils.MinPerDay;
			double tEnd = (iEnd < timedRates.size()) ? rateEnd.getTime() : rateEnd.getTime() + Utils.MinPerDay;
			double tAfterEnd = (iAfterEnd < timedRates.size()) ? rateAfterEnd.getTime() : rateAfterEnd.getTime() + Utils.MinPerDay;

			// (tPreStart, rates.get(nPreStart).getData().get_())
			// (tStart, rates.get(nStart).getData().get_())
			// (tEnd, rates.get(nEnd).getData().get_())
			// (tAfterEnd, rates.get(nAfterEnd).getData().get_())

			int tFrom = ((int) tStart >= 0) ? (int) tStart : 0;
			int tTo = ((int) tEnd < Utils.MinPerDay) ? (int) tEnd : Utils.MinPerDay - 1;

			Function<Double, Double> fK = Interpolation
					.cube(tPreStart, ratePreStart.getK(), tStart, rateStart.getK(), tEnd, rateEnd.getK(), tAfterEnd, rateAfterEnd.getK());

			for (int t = tFrom; t < tTo; t++)
			{
				list.getKoof(t).setK(fK.apply((double) t));
			}

			Function<Double, Double> fQ = Interpolation
					.cube(tPreStart, ratePreStart.getQ(), tStart, rateStart.getQ(), tEnd, rateEnd.getQ(), tAfterEnd, rateAfterEnd.getQ());

			for (int t = tFrom; t < tTo; t++)
			{
				list.getKoof(t).setQ(fQ.apply((double) t));
			}

			for (int t = tFrom; t < tTo; t++)
			{
				list.getKoof(t).setP(0.0);
			}
		}

		return list;
	}
}
