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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;
import org.bosik.merklesync.Versioned;
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
import com.googlecode.wickedcharts.wicket6.highcharts.Chart;

class BasicLineOptions extends Options
{
	private static final long	serialVersionUID	= 1L;

	@SpringBean
	private DiaryService		diaryService;

	public BasicLineOptions()
	{
		Injector.get().inject(this);

		List<Number> s1 = new ArrayList<Number>();
		List<Number> s2 = new ArrayList<Number>();
		List<Number> s3 = new ArrayList<Number>();
		List<Number> s4 = new ArrayList<Number>();

		Date dateFrom = Utils.dateLocal(2013, 1, 1);
		Date dateTo = Utils.getNextMonth(dateFrom);

		for (int i = 1; i < 13; i++)
		{
			List<Versioned<DiaryRecord>> recs = diaryService.findPeriod(dateFrom, dateTo, false);
			dateFrom = dateTo;
			dateTo = Utils.getNextMonth(dateTo);

			List<Double> val = new ArrayList<Double>();
			for (Versioned<DiaryRecord> rec : recs)
			{
				if (rec.getData() instanceof BloodRecord)
				{
					val.add(((BloodRecord)rec.getData()).getValue());
				}
			}

			double mean = Utils.getMean(val);
			double dev = Utils.getDeviation(val, mean);

			s1.add(Utils.round2(mean + dev));
			s2.add(Utils.round2(mean));
			s3.add(Utils.round2(mean - dev));
			s4.add(Utils.round2(dev));
		}

		ChartOptions chartOptions = new ChartOptions();
		chartOptions.setType(SeriesType.SPLINE);
		chartOptions.setMarginRight(130);
		chartOptions.setMarginBottom(25);
		setChartOptions(chartOptions);

		Title title = new Title("Blood Sugar");
		title.setX(-20);
		setTitle(title);

		Title subTitle = new Title("Average & Deviation");
		subTitle.setX(-20);
		setSubtitle(subTitle);

		Axis xAxis = new Axis();
		xAxis.setCategories(Arrays.asList(new String[] { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
				"Oct", "Nov", "Dec" }));
		setxAxis(xAxis);

		PlotLine plotLines = new PlotLine();
		plotLines.setValue(0f);
		plotLines.setWidth(1);
		plotLines.setColor(new HexColor("#999999"));

		Axis yAxis = new Axis();
		yAxis.setTitle(new Title("Blood sugar, mmol/l"));
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

		setPlotOptions(new PlotOptionsChoice().setArea(new PlotOptions().setStacking(Stacking.NORMAL)
				.setLineColor(new HexColor("#666666")).setLineWidth(1)));

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
}

public class StatsPage extends MasterPage
{
	private static final long	serialVersionUID	= 1L;

	public StatsPage(PageParameters parameters)
	{
		super(parameters);
		add(new Chart("chart", new BasicLineOptions()));
	}
}
