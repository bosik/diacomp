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
package org.bosik.diacomp.web.frontend.wicket.pages.stats.charts;

import com.googlecode.wickedcharts.highcharts.options.Axis;
import com.googlecode.wickedcharts.highcharts.options.ChartOptions;
import com.googlecode.wickedcharts.highcharts.options.HorizontalAlignment;
import com.googlecode.wickedcharts.highcharts.options.Legend;
import com.googlecode.wickedcharts.highcharts.options.LegendLayout;
import com.googlecode.wickedcharts.highcharts.options.Marker;
import com.googlecode.wickedcharts.highcharts.options.Options;
import com.googlecode.wickedcharts.highcharts.options.PlotLine;
import com.googlecode.wickedcharts.highcharts.options.PlotOptions;
import com.googlecode.wickedcharts.highcharts.options.PlotOptionsChoice;
import com.googlecode.wickedcharts.highcharts.options.SeriesType;
import com.googlecode.wickedcharts.highcharts.options.Stacking;
import com.googlecode.wickedcharts.highcharts.options.Title;
import com.googlecode.wickedcharts.highcharts.options.VerticalAlignment;
import com.googlecode.wickedcharts.highcharts.options.color.HexColor;
import com.googlecode.wickedcharts.highcharts.options.color.RgbaColor;
import com.googlecode.wickedcharts.highcharts.options.series.Series;
import com.googlecode.wickedcharts.highcharts.options.series.SimpleSeries;
import org.apache.wicket.Component;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.features.diary.DiaryLocalService;
import org.bosik.diacomp.web.backend.features.user.info.UserInfoService;
import org.bosik.merklesync.Versioned;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

public class AverageBSChartData extends Options
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private UserInfoService userInfoService;

	@SpringBean
	private DiaryLocalService diaryService;

	private Component context;
	private Date      firstDate;
	private Interval  interval;

	public enum Interval
	{
		DAY,
		WEEK,
		MONTH
	}

	public AverageBSChartData(Component context, Date firstDate, Interval interval)
	{
		Injector.get().inject(this);
		this.context = context;
		this.firstDate = firstDate;
		this.interval = interval;

		update();
	}

	public void setFirstDate(Date firstDate)
	{
		this.firstDate = firstDate;
		update();
	}

	public void setInterval(Interval interval)
	{
		this.interval = interval;
		update();
	}

	public void shiftForward()
	{
		setFirstDate(shiftDateForward(firstDate, interval));
	}

	public void shiftBack()
	{
		setFirstDate(shiftDateBack(firstDate, interval));
	}

	private void update()
	{
		List<Number> s1 = new ArrayList<>();
		List<Number> s2 = new ArrayList<>();
		List<Number> s3 = new ArrayList<>();
		List<Number> s4 = new ArrayList<>();

		Date dateFrom = firstDate;
		Date dateTo = shiftDateForward(dateFrom, interval);

		final List<String> legendX = new ArrayList<>();//Arrays.asList(new String[] { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" });
		final int userId = userInfoService.getCurrentUserId();

		for (int i = 0; i < 12; i++)
		{
			legendX.add(Utils.formatDateUTC(dateFrom));

			List<Versioned<DiaryRecord>> recs = diaryService.findPeriod(userId, dateFrom, dateTo, false);
			dateFrom = dateTo;
			dateTo = shiftDateForward(dateTo, interval);

			double summIns = 0;
			double summCarbs = 0;

			List<Double> val = new ArrayList<>();
			for (Versioned<DiaryRecord> rec : recs)
			{
				if (rec.getData() instanceof BloodRecord)
				{
					val.add(((BloodRecord) rec.getData()).getValue());
				}
				else if (rec.getData() instanceof InsRecord)
				{
					summIns += ((InsRecord) rec.getData()).getValue();
				}
				else if (rec.getData() instanceof MealRecord)
				{
					summCarbs += ((MealRecord) rec.getData()).getCarbs();
				}
			}

			double mean = Utils.getMean(val);
			double dev = Utils.getDeviation(val, mean);
			double x = summCarbs > Utils.EPS ? summIns / summCarbs : 0.0;

			s1.add(Utils.round2(mean + dev));
			s2.add(Utils.round2(mean));
			s3.add(Utils.round2(mean - dev));
			s4.add(/* Utils.round2(dev) */x);
		}

		ChartOptions chartOptions = new ChartOptions();
		chartOptions.setType(SeriesType.SPLINE);
		chartOptions.setMarginRight(130);
		chartOptions.setMarginBottom(35);
		setChartOptions(chartOptions);

		Title title = new Title(context.getString("chart.blood.title"));
		title.setX(-20);
		setTitle(title);

		//		Title subTitle = new Title("Average & Deviation");
		//		subTitle.setX(-20);
		//		setSubtitle(subTitle);

		Axis xAxis = new Axis();
		// FIXME: respect Interval
		xAxis.setCategories(legendX);
		setxAxis(xAxis);

		PlotLine plotLines = new PlotLine();
		plotLines.setValue(0f);
		plotLines.setWidth(1);
		plotLines.setColor(new HexColor("#999999"));

		Axis yAxis = new Axis();
		yAxis.setTitle(new Title(context.getString("chart.blood.axis.y")));
		yAxis.setPlotLines(Collections.singletonList(plotLines));
		setyAxis(yAxis);

		Legend legend = new Legend();
		legend.setLayout(LegendLayout.VERTICAL);
		legend.setAlign(HorizontalAlignment.RIGHT);
		legend.setVerticalAlign(VerticalAlignment.TOP);
		legend.setX(-10);
		legend.setY(100);
		legend.setBorderWidth(0);
		setLegend(legend);

		setPlotOptions(new PlotOptionsChoice()
				.setArea(new PlotOptions().setStacking(Stacking.NORMAL).setLineColor(new HexColor("#666666")).setLineWidth(1)));

		clearSeries();

		Series<Number> series1 = new SimpleSeries();
		series1.setName("Max");
		series1.setData(s1);
		series1.setColor(new RgbaColor(255, 0, 0, 0.25f));
		series1.setMarker(new Marker(Boolean.FALSE));
		addSeries(series1);

		Series<Number> series2 = new SimpleSeries();
		series2.setName("Average");
		series2.setData(s2);
		series2.setColor(new RgbaColor(255, 0, 0, 1.0f));
		series2.setMarker(new Marker(Boolean.FALSE));
		addSeries(series2);

		Series<Number> series3 = new SimpleSeries();
		series3.setName("Min");
		series3.setData(s3);
		series3.setColor(new RgbaColor(255, 0, 0, 0.25f));
		series3.setMarker(new Marker(Boolean.FALSE));
		addSeries(series3);

		Series<Number> series4 = new SimpleSeries();
		series4.setName("Deviation");
		series4.setData(s4);
		series4.setColor(new RgbaColor(0, 0, 0, 0.2f));
		series4.setMarker(new Marker(Boolean.FALSE));
		addSeries(series4);
	}

	private static Date shiftDateForward(Date date, Interval interval)
	{
		switch (interval)
		{
			case DAY:
			{
				return Utils.getNextDay(date);
			}
			case WEEK:
			{
				return Utils.shiftDate(date, 7);
			}
			case MONTH:
			{
				return Utils.getNextMonth(date);
			}
			default:
			{
				throw new UnsupportedOperationException("Unsupported interval: " + interval);
			}
		}
	}

	private static Date shiftDateBack(Date date, Interval interval)
	{
		switch (interval)
		{
			case DAY:
			{
				return Utils.getPrevDay(date);
			}
			case WEEK:
			{
				return Utils.shiftDate(date, -7);
			}
			case MONTH:
			{
				return Utils.getPrevMonth(date);
			}
			default:
			{
				throw new UnsupportedOperationException("Unsupported interval: " + interval);
			}
		}
	}
}
