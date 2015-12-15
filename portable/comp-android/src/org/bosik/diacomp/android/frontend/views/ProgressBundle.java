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
import com.jjoe64.graphview.series.Series;
import android.content.ContentResolver;

public class ProgressBundle
{
	public interface DataLoader
	{
		Collection<Series<?>> load(ContentResolver contentResolver);
	}

	public interface ProgressListener
	{
		void onLoading();

		void onReady(Collection<Series<?>> data);
	}

	public enum ProgressState
	{
		INITIAL, LOADING, DONE
	}

	private Collection<Series<?>>	series;
	private ProgressState			state;
	private DataLoader				dataLoader;
	private ProgressListener		listener;

	public Collection<Series<?>> getSeries()
	{
		return series;
	}

	public void setSeries(Collection<Series<?>> series)
	{
		this.series = series;
	}

	public ProgressState getState()
	{
		return state;
	}

	public void setState(ProgressState state)
	{
		this.state = state;
	}

	public DataLoader getDataLoader()
	{
		return dataLoader;
	}

	public void setDataLoader(DataLoader dataLoader)
	{
		this.dataLoader = dataLoader;
	}

	public ProgressListener getListener()
	{
		return listener;
	}

	public void setListener(ProgressListener listener)
	{
		this.listener = listener;
	}
}