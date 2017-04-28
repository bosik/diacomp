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

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.frontend.fragments.chart.ProgressBundle.DataLoader;
import org.bosik.diacomp.android.frontend.fragments.chart.ProgressBundle.ProgressListener;
import org.bosik.diacomp.android.frontend.fragments.chart.ProgressBundle.ProgressState;
import org.bosik.diacomp.android.frontend.fragments.chart.daily.PostSetupDaily;
import org.bosik.diacomp.android.frontend.fragments.chart.history.PostSetupHistory;
import com.jjoe64.graphview.GraphView;
import com.jjoe64.graphview.series.Series;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentActivity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

public class Chart extends Fragment implements ProgressListener
{
	public interface PostSetupListener
	{
		void onPostSetup(Chart chart);
	}

	public enum ChartType
	{
		HISTORY, DAILY;
	}

	// UI
	private TextView									titleView;
	private GraphView									graphView;
	private ProgressBar									progress;
	private TextView									textNoData;

	// data
	private int											chartId;
	private DataLoader									dataLoader;
	private PostSetupListener							postSetupListener;
	private String										title;
	private String										description;
	private ChartType									chartType;

	// bundle keys
	private static final String							KEY_ID			= "chart.Id";
	private static final String							KEY_CHART_TYPE	= "chart.type";
	private static final String							KEY_TITLE		= "chart.title";
	private static final String							KEY_DESCRIPTION	= "chart.description";
	private static final String							KEY_MIN_X		= "chart.minX";
	private static final String							KEY_MAX_X		= "chart.maxX";
	private static final String							KEY_MIN_Y		= "chart.minY";
	private static final String							KEY_MAX_Y		= "chart.maxY";

	private static final Map<Integer, ProgressBundle>	container		= new ConcurrentHashMap<Integer, ProgressBundle>();

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		if (savedInstanceState != null && savedInstanceState.containsKey(KEY_ID))
		{
			chartId = savedInstanceState.getInt(KEY_ID);
		}
		else
		{
			chartId = hashCode();
		}

		if (savedInstanceState != null)
		{
			if (savedInstanceState.containsKey(KEY_TITLE))
			{
				title = savedInstanceState.getString(KEY_TITLE);
			}

			if (savedInstanceState.containsKey(KEY_DESCRIPTION))
			{
				description = savedInstanceState.getString(KEY_DESCRIPTION);
			}
		}
	}

	@Override
	public void onSaveInstanceState(Bundle outState)
	{
		super.onSaveInstanceState(outState);
		outState.putInt(KEY_ID, chartId);
		outState.putString(KEY_TITLE, title);
		outState.putString(KEY_DESCRIPTION, description);
		if (chartType != null)
		{
			outState.putInt(KEY_CHART_TYPE, chartType.ordinal());
		}
		if (graphView != null)
		{
			outState.putDouble(KEY_MIN_X, graphView.getViewport().getMinX(false));
			outState.putDouble(KEY_MAX_X, graphView.getViewport().getMaxX(false));
			outState.putDouble(KEY_MIN_Y, graphView.getViewport().getMinY(false));
			outState.putDouble(KEY_MAX_Y, graphView.getViewport().getMaxY(false));
		}
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup viewGroup, Bundle savedInstanceState)
	{
		View rootView = inflater.inflate(R.layout.fragment_chart, viewGroup, false);

		// find components
		titleView = (TextView) rootView.findViewById(R.id.textChartTitle);
		graphView = (GraphView) rootView.findViewById(R.id.chart);
		progress = (ProgressBar) rootView.findViewById(R.id.progressChart);
		textNoData = (TextView) rootView.findViewById(R.id.noDataChart);

		LinearLayout buttonHelp = (LinearLayout) rootView.findViewById(R.id.buttonHelpChart);
		buttonHelp.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				if (getActivity() != null && getDescription() != null)
				{
					UIUtils.showLongTip(getActivity(), getDescription());
				}
			}
		});

		// initialize
		titleView.setText(getTitle());
		final OnClickListener listenerRefresh = new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				refresh();
			}
		};
		graphView.setOnClickListener(listenerRefresh);
		textNoData.setOnClickListener(listenerRefresh);
		rootView.findViewById(R.id.chartBackLayout).setOnClickListener(listenerRefresh);

		if (savedInstanceState != null)
		{
			if (savedInstanceState.containsKey(KEY_CHART_TYPE))
			{
				int index = savedInstanceState.getInt(KEY_CHART_TYPE);
				setChartType(ChartType.values()[index]);
			}
			if (savedInstanceState.containsKey(KEY_MIN_X))
			{
				graphView.getViewport().setMinX(savedInstanceState.getDouble(KEY_MIN_X));
				graphView.getViewport().setXAxisBoundsManual(true);
			}
			if (savedInstanceState.containsKey(KEY_MAX_X))
			{
				graphView.getViewport().setMaxX(savedInstanceState.getDouble(KEY_MAX_X));
				graphView.getViewport().setXAxisBoundsManual(true);
			}
			if (savedInstanceState.containsKey(KEY_MIN_Y))
			{
				graphView.getViewport().setMinY(savedInstanceState.getDouble(KEY_MIN_Y));
				graphView.getViewport().setYAxisBoundsManual(true);
			}
			if (savedInstanceState.containsKey(KEY_MAX_Y))
			{
				graphView.getViewport().setMaxY(savedInstanceState.getDouble(KEY_MAX_Y));
				graphView.getViewport().setYAxisBoundsManual(true);
			}
		}

		ProgressBundle bundle = getBundle();
		switch (bundle.getState())
		{
			case INITIAL:
			{
				refresh();
				break;
			}
			case LOADING:
			{
				onLoading();
				break;
			}
			case DONE:
			{
				onReady(bundle.getSeries());
				break;
			}
		}

		return rootView;
	}

	private ProgressBundle getBundle()
	{
		ProgressBundle bundle = container.get(chartId);
		if (bundle == null)
		{
			bundle = new ProgressBundle();
			bundle.setState(ProgressState.INITIAL);
			bundle.setSeries(null);
			bundle.setListener(this);
			bundle.setDataLoader(dataLoader);
			container.put(chartId, bundle);
		}
		else
		{
			bundle.setListener(this);
		}

		return bundle;
	}

	@Override
	public void onDestroyView()
	{
		super.onDestroyView();
		getBundle().setListener(null);
	}

	@Override
	public void onLoading()
	{
		graphView.setVisibility(View.GONE);
		textNoData.setVisibility(View.GONE);
		progress.setVisibility(View.VISIBLE);
	}

	@Override
	public void onReady(Collection<Series<?>> result)
	{
		progress.setVisibility(View.GONE);
		graphView.removeAllSeries();

		boolean hasData = false;
		for (Series<?> s : result)
		{
			graphView.addSeries(s);
			hasData = hasData || !s.isEmpty();
		}

		if (hasData)
		{
			graphView.setVisibility(View.VISIBLE);
			textNoData.setVisibility(View.GONE);
			if (postSetupListener != null)
			{
				postSetupListener.onPostSetup(this);
			}
		}
		else
		{
			graphView.setVisibility(View.GONE);
			textNoData.setVisibility(View.VISIBLE);
		}
	}

	public void refresh()
	{
		ProgressBundle bundle = getBundle();

		if (bundle.getDataLoader() != null && bundle.getState() != ProgressState.LOADING)
		{
			FragmentActivity activity = getActivity();
			if (activity != null)
			{
				new DataLoadingTask(bundle).execute(activity.getContentResolver());
			}
		}
	}

	public GraphView getGraphView()
	{
		return graphView;
	}

	public void setDataLoader(DataLoader dataLoader)
	{
		this.dataLoader = dataLoader;
	}

	public String getTitle()
	{
		return title;
	}

	public void setTitle(String title)
	{
		this.title = title;

		if (titleView != null)
		{
			titleView.setText(getTitle());
		}
	}

	public String getDescription()
	{
		return description;
	}

	public void setDescription(String description)
	{
		this.description = description;
	}

	public ChartType getChartType()
	{
		return chartType;
	}

	public void setChartType(ChartType chartType)
	{
		this.chartType = chartType;

		switch (chartType)
		{
			case DAILY:
			{
				postSetupListener = new PostSetupDaily();
				break;
			}
			case HISTORY:
			{
				postSetupListener = new PostSetupHistory();
				break;
			}
			default:
			{
				// ignore
				break;
			}
		}
	}
}
