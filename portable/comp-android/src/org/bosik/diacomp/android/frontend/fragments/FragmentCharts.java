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
import android.graphics.Color;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;
import com.jjoe64.graphview.GraphView;
import com.jjoe64.graphview.GraphView.GraphViewData;
import com.jjoe64.graphview.GraphView.LegendAlign;
import com.jjoe64.graphview.GraphViewSeries;
import com.jjoe64.graphview.GraphViewSeries.GraphViewSeriesStyle;
import com.jjoe64.graphview.LineGraphView;

public class FragmentCharts extends Fragment
{
	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
	{
		// TODO: check if required
		setHasOptionsMenu(false);

		// services
		final KoofService koofService = KoofServiceInternal.getInstance(getActivity().getContentResolver());

		// Widgets binding
		View rootView = inflater.inflate(R.layout.activity_graph, container, false);

		final GraphViewSeriesStyle styleK = new GraphViewSeriesStyle(Color.rgb(255, 128, 128), 4);
		final GraphViewSeriesStyle styleQ = new GraphViewSeriesStyle(Color.rgb(128, 128, 255), 4);

		// init example series data

		List<GraphViewData> dataK = new ArrayList<GraphViewData>();
		List<GraphViewData> dataQ = new ArrayList<GraphViewData>();
		for (int time = 0; time < Utils.MinPerDay; time += 10)
		{
			Koof koof = koofService.getKoof(time);
			dataK.add(new GraphViewData(time, koof.getK()));
			dataQ.add(new GraphViewData(time, koof.getQ()));
		}

		GraphViewSeries seriesK = new GraphViewSeries("Carbohydrate koof.", styleK,
				dataK.toArray(new GraphViewData[dataK.size()]));
		GraphViewSeries seriesQ = new GraphViewSeries("Insulin koof.", styleQ, dataQ.toArray(new GraphViewData[dataQ
				.size()]));

		GraphView graphView = new LineGraphView(getActivity(), "Graph");
		graphView.addSeries(seriesK);
		graphView.addSeries(seriesQ);
		graphView.setManualYMinBound(0);
		// graphView.setManualYMaxBound(1.0);
		graphView.setScalable(true);
		graphView.setScrollable(true);
		graphView.setShowLegend(true);
		graphView.setShowLegend(true);
		graphView.setLegendAlign(LegendAlign.TOP);
		// graphView.setLegendWidth(200);

		RelativeLayout layout = (RelativeLayout) rootView.findViewById(R.id.layoutGraph);
		layout.addView(graphView);

		return rootView;
	}
}