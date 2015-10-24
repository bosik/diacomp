/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.web.frontend.wicket.pages.stats;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;
import org.bosik.diacomp.web.frontend.wicket.pages.stats.charts.AverageBSChartData;
import org.bosik.diacomp.web.frontend.wicket.pages.stats.charts.AverageBSChartData.Interval;
import com.googlecode.wickedcharts.wicket6.highcharts.Chart;

public class StatsPage extends MasterPage
{
	private static final long serialVersionUID = 1L;

	public StatsPage(PageParameters parameters)
	{
		super(parameters);

		final AverageBSChartData chartBloodSugarData = new AverageBSChartData(this,
				Utils.dateLocal(getTimeZone(), 2014, 1, 1), Interval.WEEK);
		final Chart chartBloodSugar = new Chart("chart", chartBloodSugarData);
		chartBloodSugar.setOutputMarkupId(true);
		add(chartBloodSugar);

		add(new AjaxLink<Void>("linkDay")
		{
			private static final long serialVersionUID = -7873018514549864757L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				chartBloodSugarData.setInterval(Interval.DAY);
				target.add(chartBloodSugar);
			}
		});

		add(new AjaxLink<Void>("linkWeek")
		{
			private static final long serialVersionUID = 8421066200529747520L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				//chartBloodSugarData.setInterval(Interval.WEEK);
				chartBloodSugarData.shiftBack();
				target.add(chartBloodSugar);
			}
		});

		add(new AjaxLink<Void>("linkMonth")
		{
			private static final long serialVersionUID = 1446882944661826329L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				//chartBloodSugarData.setInterval(Interval.MONTH);
				chartBloodSugarData.shiftForward();
				target.add(chartBloodSugar);
			}
		});
	}
}
