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
package org.bosik.diacomp.android.frontend.fragments;

import java.util.ArrayList;
import java.util.List;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.analyze.KoofServiceInternal;
import org.bosik.diacomp.core.services.analyze.KoofService;
import org.bosik.diacomp.core.services.analyze.entities.Koof;
import org.bosik.diacomp.core.utils.Utils;
import com.jjoe64.graphview.GraphView;
import com.jjoe64.graphview.series.DataPoint;
import com.jjoe64.graphview.series.LineGraphSeries;
import android.graphics.Color;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;

public class FragmentCharts extends Fragment
{
	private static double max(List<DataPoint> values)
	{
		double max = 0;
		for (DataPoint point : values)
		{
			max = Math.max(max, point.getY());
		}

		double factor = 0.05 * 4;
		return ((int) (1.1 * max / factor) + 1) * factor;

	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
	{
		// TODO: check if required
		setHasOptionsMenu(false);

		// services
		final KoofService koofService = KoofServiceInternal.getInstance(getActivity().getContentResolver());

		// Widgets binding
		View rootView = inflater.inflate(R.layout.fragment_charts, container, false);

		// init example series data

		List<DataPoint> dataK = new ArrayList<DataPoint>();
		List<DataPoint> dataQ = new ArrayList<DataPoint>();
		List<DataPoint> dataX = new ArrayList<DataPoint>();
		for (int time = 0; time <= Utils.MinPerDay; time += 30)
		{
			Koof koof = koofService.getKoof(time);
			double t = (double) time / 60;
			dataK.add(new DataPoint(t, koof.getK()));
			dataQ.add(new DataPoint(t, koof.getQ()));
			dataX.add(new DataPoint(t, koof.getK() / koof.getQ()));
		}

		LineGraphSeries<DataPoint> seriesK = new LineGraphSeries<DataPoint>(dataK.toArray(new DataPoint[dataK.size()]));
		LineGraphSeries<DataPoint> seriesQ = new LineGraphSeries<DataPoint>(dataQ.toArray(new DataPoint[dataQ.size()]));
		LineGraphSeries<DataPoint> seriesX = new LineGraphSeries<DataPoint>(dataX.toArray(new DataPoint[dataX.size()]));

		seriesK.setColor(Color.rgb(255, 0, 0));
		seriesQ.setColor(Color.rgb(0, 0, 255));
		seriesX.setColor(Color.rgb(128, 128, 128));

		GraphView graphX = new GraphView(getActivity());
		graphX.addSeries(seriesX);
		graphX.getViewport().setXAxisBoundsManual(true);
		graphX.getViewport().setYAxisBoundsManual(true);
		graphX.getViewport().setMinX(0);
		graphX.getViewport().setMaxX(24);
		graphX.getViewport().setMinY(0);
		graphX.getViewport().setMaxY(max(dataX));
		((LinearLayout) rootView.findViewById(R.id.chartX)).addView(graphX);

		GraphView graphK = new GraphView(getActivity());
		graphK.addSeries(seriesK);
		graphK.getViewport().setXAxisBoundsManual(true);
		graphK.getViewport().setYAxisBoundsManual(true);
		graphK.getViewport().setMinX(0);
		graphK.getViewport().setMaxX(24);
		graphK.getViewport().setMinY(0);
		graphK.getViewport().setMaxY(max(dataK));
		((LinearLayout) rootView.findViewById(R.id.chartK)).addView(graphK);

		GraphView graphQ = new GraphView(getActivity());
		graphQ.addSeries(seriesQ);
		graphQ.getViewport().setXAxisBoundsManual(true);
		graphQ.getViewport().setYAxisBoundsManual(true);
		graphQ.getViewport().setMinX(0);
		graphQ.getViewport().setMaxX(24);
		graphQ.getViewport().setMinY(0);
		graphQ.getViewport().setMaxY(max(dataQ));
		((LinearLayout) rootView.findViewById(R.id.chartQ)).addView(graphQ);

		return rootView;
	}
}