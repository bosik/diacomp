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
package org.bosik.diacomp.android.frontend.fragments.chart.history;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import com.jjoe64.graphview.LabelFormatter;
import com.jjoe64.graphview.Viewport;

public class HistoryLabels implements LabelFormatter
{
	private double maxY;

	public HistoryLabels(double maxY)
	{
		this.maxY = maxY;
	}

	@Override
	public String formatLabel(double value, boolean isValueX)
	{
		if (isValueX)
		{
			return new SimpleDateFormat("dd/MM", Locale.US).format(new Date((long) value));
		}
		else
		{
			if (maxY / 4 >= 1)
			{
				return String.format(Locale.US, "%.0f", value);
			}
			else if (maxY / 4 >= 0.1)
			{
				return String.format(Locale.US, "%.1f", value);
			}
			else
			{
				return String.format(Locale.US, "%.2f", value);
			}
		}
	}

	@Override
	public void setViewport(Viewport arg0)
	{
	}
}