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
package org.bosik.diacomp.android.frontend.fragments.chart;

import java.util.List;
import org.bosik.diacomp.android.frontend.fragments.chart.Chart.PostSetupListener;
import com.jjoe64.graphview.series.Series;

public abstract class PostSetup implements PostSetupListener
{
	@SuppressWarnings("rawtypes")
	protected static Double getMaxY(List<Series> series)
	{
		Double result = null;
		for (Series<?> s : series)
		{
			if (!s.isEmpty())
			{
				if (result == null || s.getHighestValueY() > result)
				{
					result = s.getHighestValueY();
				}
			}
		}
		return result;
	}

	@SuppressWarnings("rawtypes")
	protected static Double getMinX(List<Series> series)
	{
		Double result = null;
		for (Series<?> s : series)
		{
			if (!s.isEmpty())
			{
				if (result == null || s.getLowestValueX() < result)
				{
					result = s.getLowestValueX();
				}
			}
		}
		return result;
	}

	@SuppressWarnings("rawtypes")
	protected static Double getMaxX(List<Series> series)
	{
		Double result = null;
		for (Series<?> s : series)
		{
			if (!s.isEmpty())
			{
				if (result == null || s.getHighestValueX() > result)
				{
					result = s.getHighestValueX();
				}
			}
		}
		return result;
	}

	protected static double addRoom(double max)
	{
		double top = 0.04;

		while (top < max)
		{
			if (top * 2 > max)
			{
				return top * 2;
			}
			if (top * 5 > max)
			{
				return top * 5;
			}
			top *= 10;
		}

		return top;
	}
}