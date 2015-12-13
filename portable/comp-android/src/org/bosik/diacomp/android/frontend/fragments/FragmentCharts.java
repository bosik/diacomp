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
import org.bosik.diacomp.android.frontend.views.Chart;
import org.bosik.diacomp.android.frontend.views.Chart.DataLoader;
import org.bosik.diacomp.core.services.analyze.KoofService;
import org.bosik.diacomp.core.services.analyze.entities.Koof;
import org.bosik.diacomp.core.utils.Utils;
import com.jjoe64.graphview.series.DataPoint;
import com.jjoe64.graphview.series.LineGraphSeries;
import com.jjoe64.graphview.series.Series;
import android.graphics.Color;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

public class FragmentCharts extends Fragment
{
	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
	{
		View rootView = inflater.inflate(R.layout.fragment_charts, container, false);

		addChartX();
		addChartK();
		addChartQ();

		return rootView;
	}

	static double addRoom(double max)
	{
		double factor = 0.05 * 4;
		return ((int) (1.1 * max / factor) + 1) * factor;
	}

	private void addChartX()
	{
		Chart chart = new Chart();
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public void beforeLoading(Chart chart)
			{
				chart.getTitleView().setText(getString(R.string.common_koof_x));
			}

			@Override
			public Series<?> load()
			{
				KoofService koofService = KoofServiceInternal.getInstance(getActivity().getContentResolver());

				List<DataPoint> dataList = new ArrayList<DataPoint>();
				for (int time = 0; time <= Utils.MinPerDay; time += 30)
				{
					Koof koof = koofService.getKoof(time);
					double x = (double) time / 60;
					double y = koof.getK() / koof.getQ();
					dataList.add(new DataPoint(x, y));
				}
				DataPoint[] data = dataList.toArray(new DataPoint[dataList.size()]);
				LineGraphSeries<DataPoint> series = new LineGraphSeries<DataPoint>(data);
				series.setColor(Color.rgb(128, 128, 128));

				return series;
			}

			@Override
			public void afterLoading(Chart chart)
			{
				chart.getGraphView().getViewport().setXAxisBoundsManual(true);
				chart.getGraphView().getViewport().setYAxisBoundsManual(true);
				chart.getGraphView().getViewport().setMinX(0);
				chart.getGraphView().getViewport().setMaxX(24);
				chart.getGraphView().getViewport().setMinY(0);

				double y = chart.getGraphView().getSeries().get(0).getHighestValueY();
				chart.getGraphView().getViewport().setMaxY(addRoom(y));
			}
		});
		getChildFragmentManager().beginTransaction().replace(R.id.chartX, chart).commit();
	}

	private void addChartK()
	{
		Chart chart = new Chart();
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public void beforeLoading(Chart chart)
			{
				chart.getTitleView().setText(getString(R.string.common_koof_k));
			}

			@Override
			public Series<?> load()
			{
				KoofService koofService = KoofServiceInternal.getInstance(getActivity().getContentResolver());

				List<DataPoint> dataList = new ArrayList<DataPoint>();
				for (int time = 0; time <= Utils.MinPerDay; time += 30)
				{
					double x = (double) time / 60;
					double y = koofService.getKoof(time).getK();
					dataList.add(new DataPoint(x, y));
				}
				DataPoint[] data = dataList.toArray(new DataPoint[dataList.size()]);
				LineGraphSeries<DataPoint> series = new LineGraphSeries<DataPoint>(data);
				series.setColor(Color.rgb(255, 0, 0));

				return series;
			}

			@Override
			public void afterLoading(Chart chart)
			{
				chart.getGraphView().getViewport().setXAxisBoundsManual(true);
				chart.getGraphView().getViewport().setYAxisBoundsManual(true);
				chart.getGraphView().getViewport().setMinX(0);
				chart.getGraphView().getViewport().setMaxX(24);
				chart.getGraphView().getViewport().setMinY(0);

				double y = chart.getGraphView().getSeries().get(0).getHighestValueY();
				chart.getGraphView().getViewport().setMaxY(addRoom(y));
			}
		});
		getChildFragmentManager().beginTransaction().replace(R.id.chartK, chart).commit();
	}

	private void addChartQ()
	{
		Chart chart = new Chart();
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public void beforeLoading(Chart chart)
			{
				chart.getTitleView().setText(getString(R.string.common_koof_q));
			}

			@Override
			public Series<?> load()
			{
				KoofService koofService = KoofServiceInternal.getInstance(getActivity().getContentResolver());

				List<DataPoint> dataList = new ArrayList<DataPoint>();
				for (int time = 0; time <= Utils.MinPerDay; time += 30)
				{
					double x = (double) time / 60;
					double y = koofService.getKoof(time).getQ();
					dataList.add(new DataPoint(x, y));
				}
				DataPoint[] data = dataList.toArray(new DataPoint[dataList.size()]);
				LineGraphSeries<DataPoint> series = new LineGraphSeries<DataPoint>(data);
				series.setColor(Color.rgb(0, 0, 255));

				return series;
			}

			@Override
			public void afterLoading(Chart chart)
			{
				chart.getGraphView().getViewport().setXAxisBoundsManual(true);
				chart.getGraphView().getViewport().setYAxisBoundsManual(true);
				chart.getGraphView().getViewport().setMinX(0);
				chart.getGraphView().getViewport().setMaxX(24);
				chart.getGraphView().getViewport().setMinY(0);

				double y = chart.getGraphView().getSeries().get(0).getHighestValueY();
				chart.getGraphView().getViewport().setMaxY(addRoom(y));
			}
		});
		getChildFragmentManager().beginTransaction().replace(R.id.chartQ, chart).commit();
	}
}