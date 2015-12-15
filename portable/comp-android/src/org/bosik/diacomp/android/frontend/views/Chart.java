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

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.frontend.views.ProgressBundle.DataLoader;
import org.bosik.diacomp.android.frontend.views.ProgressBundle.ProgressListener;
import org.bosik.diacomp.android.frontend.views.ProgressBundle.ProgressState;
import com.jjoe64.graphview.GraphView;
import com.jjoe64.graphview.series.Series;
import android.content.ContentResolver;
import android.os.AsyncTask;
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

class MyAsyncTask extends AsyncTask<ContentResolver, Void, Collection<Series<?>>>
{
	private ProgressBundle bundle;

	public MyAsyncTask(ProgressBundle bundle)
	{
		this.bundle = bundle;
	}

	@Override
	protected void onPreExecute()
	{
		bundle.setState(ProgressState.LOADING);
		if (bundle.getListener() != null)
		{
			bundle.getListener().onLoading();
		}
	};

	@Override
	protected Collection<Series<?>> doInBackground(ContentResolver... params)
	{
		if (bundle.getDataLoader() != null)
		{
			return bundle.getDataLoader().load(params[0]);
		}
		else
		{
			return Collections.emptyList();
		}
	}

	@Override
	protected void onPostExecute(Collection<Series<?>> result)
	{
		bundle.setSeries(result);
		bundle.setState(ProgressState.DONE);
		if (bundle.getListener() != null)
		{
			bundle.getListener().onReady(result);
		}
	};
}

public class Chart extends Fragment implements ProgressListener
{
	public interface PostSetupListener
	{
		void onPostSetup(Chart chart);
	}

	// UI
	private TextView									titleView;
	private GraphView									graphView;
	private ProgressBar									progress;

	// data
	private int											chartId;
	private DataLoader									dataLoader;
	private PostSetupListener							postSetupListener;
	private String										title;
	private String										description;

	private static final String							KEY_ID		= "id";
	private static final Map<Integer, ProgressBundle>	container	= new ConcurrentHashMap<Integer, ProgressBundle>();

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
	}

	@Override
	public void onSaveInstanceState(Bundle outState)
	{
		super.onSaveInstanceState(outState);
		outState.putInt(KEY_ID, chartId);
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup viewGroup, Bundle savedInstanceState)
	{
		View rootView = inflater.inflate(R.layout.fragment_chart, viewGroup, false);

		// find components
		titleView = (TextView) rootView.findViewById(R.id.textChartTitle);
		graphView = (GraphView) rootView.findViewById(R.id.chart);
		progress = (ProgressBar) rootView.findViewById(R.id.progressChart);

		LinearLayout buttonHelp = (LinearLayout) rootView.findViewById(R.id.buttonHelpChart);
		buttonHelp.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				if (getActivity() != null && getDescription() != null)
				{
					UIUtils.showTip(getActivity(), getDescription());
				}
			}
		});

		// initialize
		titleView.setText(getTitle());
		graphView.setOnClickListener(new OnClickListener()
		{
			// FIXME: test only
			@Override
			public void onClick(View v)
			{
				refresh();
			}
		});

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
		progress.setVisibility(View.VISIBLE);
	}

	@Override
	public void onReady(Collection<Series<?>> result)
	{
		graphView.setVisibility(View.VISIBLE);
		progress.setVisibility(View.GONE);
		graphView.removeAllSeries();
		for (Series<?> s : result)
		{
			graphView.addSeries(s);
		}

		if (postSetupListener != null)
		{
			postSetupListener.onPostSetup(this);
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
				MyAsyncTask asyncTask = new MyAsyncTask(bundle);
				asyncTask.execute(activity.getContentResolver());
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
	}

	public String getDescription()
	{
		return description;
	}

	public void setDescription(String description)
	{
		this.description = description;
	}

	public void setPostSetupListener(PostSetupListener postSetupListener)
	{
		this.postSetupListener = postSetupListener;
	}
}
