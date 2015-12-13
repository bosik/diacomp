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
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.LinearLayout;
import android.widget.TextView;

public class Chart extends LinearLayout
{
	private TextView	titleView;
	private GraphView	graphView;

	public Chart(final Context context)
	{
		this(context, null);
	}

	public Chart(final Context context, AttributeSet attributes)
	{
		super(context, attributes);

		if (isInEditMode())
		{
			setMinimumHeight(200);
			return;
		}

		final LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.view_chart, this);

		titleView = (TextView) findViewById(R.id.textChartTitle);
		graphView = (GraphView) findViewById(R.id.chart);
	}

	public TextView getTitleView()
	{
		return titleView;
	}

	public GraphView getGraphView()
	{
		return graphView;
	}
}
