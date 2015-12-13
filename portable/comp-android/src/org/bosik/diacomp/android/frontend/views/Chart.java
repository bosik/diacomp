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
package org.bosik.diacomp.android.frontend.views;

import org.bosik.diacomp.android.R;
import com.jjoe64.graphview.GraphView;
import com.jjoe64.graphview.series.Series;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ProgressBar;
import android.widget.TextView;

public class Chart extends Fragment
{
	public interface DataLoader
	{
		void beforeLoading(Chart chart);

		Series<?> load();

		void afterLoading(Chart chart);
	}

	// UI
	private TextView	titleView;
	GraphView			graphView;
	ProgressBar			progress;

	// data
	DataLoader			dataLoader;

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
	{
		View rootView = inflater.inflate(R.layout.fragment_chart, container, false);

		titleView = (TextView) rootView.findViewById(R.id.textChartTitle);
		graphView = (GraphView) rootView.findViewById(R.id.chart);
		progress = (ProgressBar) rootView.findViewById(R.id.progressChart);

		return rootView;
	}

	@Override
	public void onResume()
	{
		super.onResume();

		if (dataLoader != null)
		{
			new AsyncTask<Void, Void, Series<?>>()
			{
				@Override
				protected void onPreExecute()
				{
					graphView.setVisibility(View.GONE);
					progress.setVisibility(View.VISIBLE);

					dataLoader.beforeLoading(Chart.this);
				};

				@Override
				protected Series<?> doInBackground(Void... params)
				{
					return dataLoader.load();
				}

				@Override
				protected void onPostExecute(Series<?> result)
				{
					graphView.setVisibility(View.VISIBLE);
					progress.setVisibility(View.GONE);

					graphView.removeAllSeries();
					graphView.addSeries(result);
					dataLoader.afterLoading(Chart.this);
				};

			}.execute();
		}
	}

	// @Override
	// public void onActivityCreated(Bundle savedInstanceState)
	// {
	// super.onActivityCreated(savedInstanceState);
	//
	// boolean executed = savedInstanceState != null && savedInstanceState.getBoolean("executed",
	// false);
	//
	// if (onLoadListener != null && !executed)
	// {
	// onLoadListener.onLoad(this);
	// System.out.println("Runned");
	// if (savedInstanceState != null)
	// {
	// savedInstanceState.putBoolean("executed", true);
	// }
	// }
	// }

	public TextView getTitleView()
	{
		return titleView;
	}

	public GraphView getGraphView()
	{
		return graphView;
	}

	public void setDataLoader(DataLoader dataLoader)
	{
		this.dataLoader = dataLoader;
	}
}
