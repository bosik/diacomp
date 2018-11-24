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

import android.content.Context;
import android.os.AsyncTask;
import com.jjoe64.graphview.series.Series;
import org.bosik.diacomp.android.frontend.fragments.chart.ProgressBundle.ProgressState;

import java.util.Collection;
import java.util.Collections;

class DataLoadingTask extends AsyncTask<Context, Void, Collection<Series<?>>>
{
	private ProgressBundle bundle;

	public DataLoadingTask(ProgressBundle bundle)
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
	}

	@Override
	protected Collection<Series<?>> doInBackground(Context... params)
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
	}
}