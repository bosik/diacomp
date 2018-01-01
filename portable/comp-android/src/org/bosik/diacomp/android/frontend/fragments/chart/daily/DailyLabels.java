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
package org.bosik.diacomp.android.frontend.fragments.chart.daily;

import com.jjoe64.graphview.LabelFormatter;
import com.jjoe64.graphview.Viewport;

import java.util.Locale;

class DailyLabels implements LabelFormatter
{
	private double maxY;

	public DailyLabels(double maxY)
	{
		this.maxY = maxY;
	}

	@Override
	public String formatLabel(double value, boolean isValueX)
	{
		if (isValueX)
		{
			return String.format(Locale.US, "%.0f", value);
		}
		else
		{
			return formatYValue(value, maxY / 4);
		}
	}

	private static String formatYValue(double value, double section)
	{
		int digits;

		if (section >= 1)
		{
			digits = 0;
		}
		else if (section >= 0.1)
		{
			digits = 1;
		}
		else if (section >= 0.01)
		{
			digits = 2;
		}
		else
		{
			digits = 3;
		}

		return String.format(Locale.US, "%." + digits + "f", value);
	}

	@Override
	public void setViewport(Viewport arg0)
	{
	}
}