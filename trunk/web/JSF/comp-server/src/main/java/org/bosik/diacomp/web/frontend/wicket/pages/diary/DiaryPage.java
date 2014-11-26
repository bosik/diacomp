package org.bosik.diacomp.web.frontend.wicket.pages.diary;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.RefreshingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.features.diary.service.FrontendDiaryService;
import org.bosik.diacomp.web.frontend.wicket.components.diary.day.DiaryPanelDay;
import org.bosik.diacomp.web.frontend.wicket.components.diary.day.DiaryPanelDayModelObject;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;
import com.googlecode.wickedcharts.highcharts.options.Axis;
import com.googlecode.wickedcharts.highcharts.options.AxisType;
import com.googlecode.wickedcharts.highcharts.options.ChartOptions;
import com.googlecode.wickedcharts.highcharts.options.Legend;
import com.googlecode.wickedcharts.highcharts.options.Marker;
import com.googlecode.wickedcharts.highcharts.options.MinorTickInterval;
import com.googlecode.wickedcharts.highcharts.options.Options;
import com.googlecode.wickedcharts.highcharts.options.PlotLine;
import com.googlecode.wickedcharts.highcharts.options.PlotOptions;
import com.googlecode.wickedcharts.highcharts.options.PlotOptionsChoice;
import com.googlecode.wickedcharts.highcharts.options.SeriesType;
import com.googlecode.wickedcharts.highcharts.options.Title;
import com.googlecode.wickedcharts.highcharts.options.color.HexColor;
import com.googlecode.wickedcharts.highcharts.options.color.RgbaColor;
import com.googlecode.wickedcharts.highcharts.options.series.Coordinate;
import com.googlecode.wickedcharts.highcharts.options.series.CoordinatesSeries;
import com.googlecode.wickedcharts.highcharts.options.series.Series;
import com.googlecode.wickedcharts.wicket6.highcharts.Chart;

class BloodOptions extends Options
{
	private static final long	serialVersionUID	= 1L;

	public BloodOptions(IModel<DiaryPanelDayModelObject> model)
	{
		List<Coordinate<Number, Number>> s1 = new ArrayList<Coordinate<Number, Number>>();

		for (Versioned<DiaryRecord> item : model.getObject().getItems())
		{
			if (item.getData() instanceof BloodRecord)
			{
				double x = ((BloodRecord)item.getData()).getTime().getTime();
				double y = ((BloodRecord)item.getData()).getValue();
				s1.add(new Coordinate<Number, Number>(x, y));
			}
		}

		Calendar cal = Calendar.getInstance();
		cal.set(Calendar.YEAR, 2013);
		cal.set(Calendar.MONTH, 0);
		cal.set(Calendar.DAY_OF_MONTH, 1);

		ChartOptions chartOptions = new ChartOptions();
		chartOptions.setType(SeriesType.SPLINE);
		//chartOptions.setMarginRight(130);
		//chartOptions.setMarginBottom(25);
		setChartOptions(chartOptions);

		Title title = new Title("Blood Sugar");
		setTitle(title);
		long start = cal.getTimeInMillis();
		long end = start + Utils.MsecPerDay;

		setxAxis(new Axis(AxisType.DATETIME).setMin(start).setMax(end));

		PlotLine plotLines = new PlotLine();
		plotLines.setValue(0f);
		plotLines.setWidth(1);
		plotLines.setColor(new HexColor("#999999"));

		Axis yAxis = new Axis();
		yAxis.setTitle(new Title("mmol/l"));
		yAxis.setPlotLines(Collections.singletonList(plotLines));
		yAxis.setMin(0);
		yAxis.setMinorTickInterval(new MinorTickInterval().setInterval(1));
		setyAxis(yAxis);

		Legend legend = new Legend();
		legend.setEnabled(false);
		setLegend(legend);

		PlotOptions plotOptions = new PlotOptions().setLineWidth(1).setPointStart(start);
		setPlotOptions(new PlotOptionsChoice().setSpline(plotOptions));

		Series<Coordinate<Number, Number>> series1 = new CoordinatesSeries();
		series1.setData(s1);
		series1.setColor(new RgbaColor(128, 0, 0, 1.0f));
		series1.setMarker(new Marker(Boolean.FALSE));
		addSeries(series1);
	}
}

public class DiaryPage extends MasterPage
{
	private static final long						serialVersionUID	= 1L;

	WebMarkupContainer								container;
	transient static final DiaryService				diaryService		= new FrontendDiaryService();
	final List<IModel<DiaryPanelDayModelObject>>	list				= new ArrayList<IModel<DiaryPanelDayModelObject>>();

	public DiaryPage(final PageParameters parameters)
	{
		super(parameters);

		list.clear();
		for (int i = 1; i <= 40; i++)
		{
			Date dateFrom = Utils.dateLocal(2013, 1, i);
			Date dateTo = Utils.getNextDay(dateFrom);
			List<Versioned<DiaryRecord>> day = diaryService.findPeriod(dateFrom, dateTo, false);
			DiaryPanelDayModelObject mo = new DiaryPanelDayModelObject(dateFrom, day);
			list.add(Model.of(mo));
		}

		add(new Chart("chart", new BloodOptions(list.get(0))));

		container = new WebMarkupContainer("wrapper");
		container.setOutputMarkupId(true);
		add(container);

		container.add(new RefreshingView<DiaryPanelDayModelObject>("diaryDay")
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected Iterator<IModel<DiaryPanelDayModelObject>> getItemModels()
			{
				//				FoodDataProvider provider = new FoodDataProvider();
				//				Iterator<? extends Versioned<FoodItem>> it = provider.iterator(0, 19);
				//				while (it.hasNext())
				//				{
				//					foodBase.add(provider.model(it.next()));
				//				}
				return list.iterator();
			}

			@Override
			protected void populateItem(final Item<DiaryPanelDayModelObject> item)
			{
				item.add(new DiaryPanelDay("diaryDayPanel", item.getModelObject()));
			}
		});
	}
}
