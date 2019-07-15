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

import com.jjoe64.graphview.GraphView;
import org.bosik.diacomp.android.frontend.fragments.chart.Chart;
import org.bosik.diacomp.android.frontend.fragments.chart.PostSetup;

public class PostSetupDaily extends PostSetup
{
	@Override
	public void onPostSetup(Chart chart)
	{
		final GraphView graphView = chart.getGraphView();

		double minX = 0.0;
		double maxX = 24.0;
		Double maxY = getMaxY(graphView.getSeries());

		if (maxY != null)
		{
			maxY = addRoom(maxY);
		}
		else
		{
			maxY = 1.0;
		}

		graphView.getViewport().setXAxisBoundsManual(true);
		graphView.getViewport().setYAxisBoundsManual(true);

		graphView.getViewport().setMinX(minX);
		graphView.getViewport().setMaxX(maxX);
		graphView.getViewport().setMinY(0);
		graphView.getViewport().setMaxY(maxY);

		graphView.getGridLabelRenderer().setLabelFormatter(new DailyLabels(maxY));

		// For the latest GraphView version, these adjustments may be required:
		//		graphView.getViewport().setXAxisBoundsStatus(Viewport.AxisBoundsStatus.FIX);
		//		graphView.getViewport().setYAxisBoundsStatus(Viewport.AxisBoundsStatus.FIX);
		//		graphView.getGridLabelRenderer().setHumanRounding(false);
	}
}