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
package org.bosik.diacomp.web.frontend.wicket.pages.diary;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxFallbackButton;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.RefreshingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.string.StringValue;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.frontend.wicket.components.diary.day.DiaryPanelDay;
import org.bosik.diacomp.web.frontend.wicket.components.diary.day.DiaryPanelDayModelObject;
import org.bosik.diacomp.web.frontend.wicket.dialogs.diary.blood.DiaryEditorBlood;
import org.bosik.diacomp.web.frontend.wicket.pages.master.MasterPage;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.Versioned;
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
		cal.setTime(model.getObject().getDate());

		ChartOptions chartOptions = new ChartOptions();
		chartOptions.setType(SeriesType.SPLINE);
		//chartOptions.setMarginRight(130);
		//chartOptions.setMarginBottom(25);
		setChartOptions(chartOptions);

		Title title = new Title("Сахар крови");
		setTitle(title);
		long start = cal.getTimeInMillis();
		long end = start + Utils.MsecPerDay;

		setxAxis(new Axis(AxisType.DATETIME).setMin(start).setMax(end).setOffset(5));

		PlotLine plotLines = new PlotLine();
		plotLines.setValue(0f);
		plotLines.setWidth(1);
		plotLines.setColor(new HexColor("#999999"));

		Axis yAxis = new Axis();
		yAxis.setTitle(new Title("ммоль/л"));
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

	private WebMarkupContainer						container;
	@SpringBean
	DiaryService									diaryService;
	final List<IModel<DiaryPanelDayModelObject>>	list				= new ArrayList<IModel<DiaryPanelDayModelObject>>();

	public DiaryPage(final PageParameters parameters)
	{
		super(parameters);

		//===========================================
		Date dateFrom = Utils.today(getTimeZone());
		Date dateTo = Utils.shiftDate(dateFrom, +1);

		try
		{
			StringValue parDateFrom = parameters.get("from");
			StringValue parDateTo = parameters.get("to");
			if (!parDateFrom.isEmpty() && !parDateTo.isEmpty())
			{
				dateFrom = Utils.parseDateLocal(getTimeZone(), parDateFrom.toString());
				dateTo = Utils.parseDateLocal(getTimeZone(), parDateTo.toString());
			}
		}
		catch (ParseException e)
		{
			// just ignore invalid parameter
		}
		//===================================================

		final DiaryEditorBlood bloodEditor = new DiaryEditorBlood("bloodEditor")
		{
			private static final long	serialVersionUID	= -8842868450540695476L;

			@Override
			public void onSave(AjaxRequestTarget target, IModel<Versioned<BloodRecord>> model)
			{
				diaryService.save(Arrays.asList(new Versioned<DiaryRecord>(model.getObject())));
				close(target);
			}

			@Override
			public void onCancel(AjaxRequestTarget target)
			{
				close(target);
			}
		};
		add(bloodEditor);

		list.clear();
		//for (int i = 1; i <= 40; i++)
		{
			//Date dateFrom = date;//Utils.dateLocal(2015, 3, i);
			//Date dateTo = Utils.getNextDay(dateFrom);
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
				item.add(new DiaryPanelDay("diaryDayPanel", item.getModel()));
			}
		});

		Form<Void> form = new Form<Void>("formNew");
		add(form);

		form.add(new AjaxFallbackButton("buttonAddBlood", form)
		{
			private static final long	serialVersionUID	= 1417984638165989821L;

			@Override
			protected void onSubmit(AjaxRequestTarget target, Form<?> form)
			{
				super.onSubmit(target, form);
				BloodRecord data = new BloodRecord();
				data.setTime(new Date());
				Versioned<BloodRecord> rec = new Versioned<BloodRecord>(data);
				rec.setId(HashUtils.generateGuid());
				bloodEditor.show(target, Model.of(rec));
			}
		});
	}
}
