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
package org.bosik.diacomp.android.frontend.fragments;

import android.content.Context;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import com.jjoe64.graphview.series.DataPoint;
import com.jjoe64.graphview.series.LineGraphSeries;
import com.jjoe64.graphview.series.Series;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.AccountUtils;
import org.bosik.diacomp.android.backend.features.analyze.RateServiceInternal;
import org.bosik.diacomp.android.backend.features.diary.LocalDiary;
import org.bosik.diacomp.android.frontend.fragments.chart.Chart;
import org.bosik.diacomp.android.frontend.fragments.chart.Chart.ChartType;
import org.bosik.diacomp.android.frontend.fragments.chart.ProgressBundle.DataLoader;
import org.bosik.diacomp.android.frontend.views.expandable.ExpandableView;
import org.bosik.diacomp.android.frontend.views.expandable.OnSwitchedListener;
import org.bosik.diacomp.core.entities.business.BloodSugarUnit;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.services.analyze.RateService;
import org.bosik.diacomp.core.services.analyze.entities.Rate;
import org.bosik.diacomp.core.services.analyze.entities.WeightedValue;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

public class FragmentTabCharts extends Fragment
{
	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
	{
		setHasOptionsMenu(true);

		View rootView = inflater.inflate(R.layout.fragment_tab_charts, container, false);

		ExpandableView groupHistory = rootView.findViewById(R.id.chartGroupHistoryTitle);
		LinearLayout groupHistoryContent = rootView.findViewById(R.id.chartGroupHistoryContent);
		groupHistory.setTitle(getActivity().getString(R.string.charts_group_history));
		groupHistory.setContentPanel(groupHistoryContent);
		groupHistory.setOnSwitchedListener(new OnSwitchedListener()
		{
			@Override
			public void onExpanded()
			{
				addChartBS(R.id.chartBS);
				addChartInsulinConsumption(R.id.chartInsulin);
				addChartCalories(R.id.chartCalories);
			}
		});

		ExpandableView groupDaily = rootView.findViewById(R.id.chartGroupDailyTitle);
		LinearLayout groupDailyContent = rootView.findViewById(R.id.chartGroupDailyContent);
		groupDaily.setTitle(getActivity().getString(R.string.charts_group_daily));
		groupDaily.setContentPanel(groupDailyContent);
		groupDaily.setOnSwitchedListener(new OnSwitchedListener()
		{
			@Override
			public void onExpanded()
			{
				addChartX(R.id.chartX);
				addChartK(R.id.chartK);
				addChartQ(R.id.chartQ);
			}
		});

		return rootView;
	}

	@Override
	public void onCreateOptionsMenu(Menu menu, MenuInflater inflater)
	{
		super.onCreateOptionsMenu(menu, inflater);
		inflater.inflate(R.menu.actions_charts, menu);

		if (AccountUtils.hasAccount(getActivity()))
		{
			MenuItem item = menu.findItem(R.id.item_common_login);
			if (item != null)
			{
				item.setVisible(false);
			}
		}
	}

	private static final int            HALF_WINDOW_SIZE = 2;    // days
	private static final int            PERIOD           = 30;    // days
	private static final BloodSugarUnit BLOOD_SUGAR_UNIT = BloodSugarUnit.MMOL_L;

	private void addChartBS(int viewId)
	{
		Chart chart = (Chart) getChildFragmentManager().findFragmentById(viewId);

		if (chart == null)
		{
			chart = new Chart();
			getChildFragmentManager().beginTransaction().add(viewId, chart).commit();
		}

		// don't call getResources() on background thread
		final int COLOR_AVERAGE = getResources().getColor(R.color.charts_bs_average);
		final int COLOR_DISPERSION = getResources().getColor(R.color.charts_bs_dispersion);

		chart.setChartType(ChartType.HISTORY);
		chart.setTitle(String.format("%s, %s", getString(R.string.charts_average_bs), getBloodSugarUnitName()));
		chart.setDescription(getString(R.string.charts_average_bs_description) + ". " + getString(R.string.charts_type_history) + ".");
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public Collection<Series<?>> load(Context context)
			{
				// PREPARE DATE TREE

				DiaryService diary = LocalDiary.getInstance(context);
				final Date todayMidnight = Utils.startOfDay(new Date());
				final Date startTime = Utils.shiftDate(todayMidnight, -PERIOD - HALF_WINDOW_SIZE);
				final Date endTime = Utils.shiftDate(todayMidnight, -HALF_WINDOW_SIZE);
				List<Versioned<DiaryRecord>> recs = diary
						.findPeriod(Utils.shiftDate(startTime, -HALF_WINDOW_SIZE), Utils.shiftDate(endTime, HALF_WINDOW_SIZE), false);

				SortedMap<Date, Double> bs = new TreeMap<>();
				for (Versioned<DiaryRecord> rec : recs)
				{
					if (rec.getData() instanceof BloodRecord)
					{
						BloodRecord blood = (BloodRecord) rec.getData();
						bs.put(blood.getTime(), BloodSugarUnit.convert(blood.getValue(), BloodSugarUnit.MMOL_L, BLOOD_SUGAR_UNIT));
					}
				}

				// ANALYZE & FILL SERIES

				List<DataPoint> dataAvg = new ArrayList<>();
				List<DataPoint> dataMin = new ArrayList<>();
				List<DataPoint> dataMax = new ArrayList<>();

				for (int i = 0; i <= PERIOD; i++)
				{
					Date windowStart = Utils.shiftDate(startTime, i - HALF_WINDOW_SIZE);
					Date windowMiddle = Utils.shiftDate(startTime, i);
					Date windowEnd = Utils.shiftDate(startTime, i + HALF_WINDOW_SIZE);
					SortedMap<Date, Double> items = bs.subMap(windowStart, windowEnd);

					if (!items.isEmpty())
					{
						final List<Date> keys = new ArrayList<>(items.keySet());
						final List<WeightedValue> points = new ArrayList<>();

						if (keys.size() == 1)
						{
							points.add(new WeightedValue(items.get(keys.get(0)), 1.0));
						}
						else
						{
							for (int j = 0; j < keys.size() - 1; j++)
							{
								final double value = (items.get(keys.get(j)) + items.get(keys.get(j + 1))) / 2;
								final double weight = keys.get(j + 1).getTime() - keys.get(j).getTime();
								points.add(new WeightedValue(value, weight));
							}
						}

						double mean = Utils.getWeightedMean(points);
						double deviation = Utils.getWeightedDeviation(points, mean);
						dataAvg.add(new DataPoint(windowMiddle, mean));
						dataMin.add(new DataPoint(windowMiddle, mean - deviation));
						dataMax.add(new DataPoint(windowMiddle, mean + deviation));
					}
				}

				LineGraphSeries<DataPoint> seriesAvg = new LineGraphSeries<>(dataAvg.toArray(new DataPoint[0]));
				seriesAvg.setColor(COLOR_AVERAGE);

				LineGraphSeries<DataPoint> seriesMin = new LineGraphSeries<>(dataMin.toArray(new DataPoint[0]));
				seriesMin.setColor(COLOR_DISPERSION);

				LineGraphSeries<DataPoint> seriesMax = new LineGraphSeries<>(dataMax.toArray(new DataPoint[0]));
				seriesMax.setColor(COLOR_DISPERSION);

				return Arrays.<Series<?>>asList(seriesMin, seriesAvg, seriesMax);
			}
		});
	}

	private String getBloodSugarUnitName()
	{
		switch (BLOOD_SUGAR_UNIT)
		{
			case MMOL_L:
				return getString(R.string.common_unit_bs_mmoll);
			case MG_DL:
				return getString(R.string.common_unit_bs_mgdl);
			default:
				throw new UnsupportedOperationException("Unsupported blood sugar unit: " + BLOOD_SUGAR_UNIT);
		}
	}

	private void addChartInsulinConsumption(int viewId)
	{
		Chart chart = (Chart) getChildFragmentManager().findFragmentById(viewId);
		if (chart == null)
		{
			chart = new Chart();
			getChildFragmentManager().beginTransaction().add(viewId, chart).commit();
		}

		// don't call getResources() on background thread
		final int COLOR = getResources().getColor(R.color.charts_insulin_consumption);

		chart.setChartType(ChartType.HISTORY);
		chart.setTitle(String.format("%s, %s/%s", getString(R.string.common_rate_x), getString(R.string.common_unit_insulin),
				getString(R.string.common_unit_mass_gramm)));
		chart.setDescription(getString(R.string.common_rate_x_description) + ". " + getString(R.string.charts_type_history) + ".");
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public Collection<Series<?>> load(Context context)
			{
				// PREPARE DATE TREE

				DiaryService diary = LocalDiary.getInstance(context);
				final Date todayMidnight = Utils.startOfDay(new Date());
				final Date startTime = Utils.shiftDate(todayMidnight, -PERIOD - HALF_WINDOW_SIZE);
				final Date endTime = Utils.shiftDate(todayMidnight, -HALF_WINDOW_SIZE);
				List<Versioned<DiaryRecord>> recs = diary
						.findPeriod(Utils.shiftDate(startTime, -HALF_WINDOW_SIZE), Utils.shiftDate(endTime, HALF_WINDOW_SIZE), false);

				SortedMap<Date, DiaryRecord> bs = new TreeMap<>();
				for (Versioned<DiaryRecord> rec : recs)
				{
					bs.put(rec.getData().getTime(), rec.getData());
				}

				// ANALYZE & FILL SERIES

				List<DataPoint> data = new ArrayList<>();

				for (int i = 0; i <= PERIOD; i++)
				{
					Date windowStart = Utils.shiftDate(startTime, i - HALF_WINDOW_SIZE);
					Date windowMiddle = Utils.shiftDate(startTime, i);
					Date windowEnd = Utils.shiftDate(startTime, i + HALF_WINDOW_SIZE);
					SortedMap<Date, DiaryRecord> items = bs.subMap(windowStart, windowEnd);

					if (!items.isEmpty())
					{
						Collection<DiaryRecord> values = items.values();

						double sumCarbs = 0.0;
						double sumInsulin = 0.0;

						for (DiaryRecord rec : values)
						{
							if (rec instanceof MealRecord)
							{
								sumCarbs += ((MealRecord) rec).getCarbs();
							}
							else if (rec instanceof InsRecord)
							{
								sumInsulin += ((InsRecord) rec).getValue();
							}
						}

						if (sumCarbs > Utils.EPS)
						{
							data.add(new DataPoint(windowMiddle, sumInsulin / sumCarbs));
						}
					}
				}

				LineGraphSeries<DataPoint> series = new LineGraphSeries<>(data.toArray(new DataPoint[0]));
				series.setColor(COLOR);

				return Collections.<Series<?>>singletonList(series);
			}
		});
	}

	private void addChartCalories(int viewId)
	{
		Chart chart = (Chart) getChildFragmentManager().findFragmentById(viewId);

		if (chart == null)
		{
			chart = new Chart();
			getChildFragmentManager().beginTransaction().add(viewId, chart).commit();
		}

		// don't call getResources() on background thread
		final int COLOR = getResources().getColor(R.color.charts_value);

		chart.setChartType(ChartType.HISTORY);
		chart.setTitle(String.format("%s, %s", getString(R.string.charts_average_calories), getString(R.string.common_unit_value_kcal)));
		chart.setDescription(
				getString(R.string.charts_average_calories_description) + ". " + getString(R.string.charts_type_history) + ".");
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public Collection<Series<?>> load(Context context)
			{
				// PREPARE DATE TREE

				DiaryService diary = LocalDiary.getInstance(context);
				final Date todayMidnight = Utils.startOfDay(new Date());
				final Date startTime = Utils.shiftDate(todayMidnight, -PERIOD - HALF_WINDOW_SIZE);
				final Date endTime = Utils.shiftDate(todayMidnight, -HALF_WINDOW_SIZE);
				List<Versioned<DiaryRecord>> recs = diary
						.findPeriod(Utils.shiftDate(startTime, -HALF_WINDOW_SIZE), Utils.shiftDate(endTime, HALF_WINDOW_SIZE), false);

				SortedMap<Date, Double> bs = new TreeMap<>();
				for (Versioned<DiaryRecord> rec : recs)
				{
					if (rec.getData() instanceof MealRecord)
					{
						MealRecord meal = (MealRecord) rec.getData();
						bs.put(meal.getTime(), meal.getValue());
					}
				}

				// ANALYZE & FILL SERIES

				List<DataPoint> dataAvg = new ArrayList<>();

				for (int i = 0; i <= PERIOD; i++)
				{
					Date windowStart = Utils.shiftDate(startTime, i - HALF_WINDOW_SIZE);
					Date windowMiddle = Utils.shiftDate(startTime, i);
					Date windowEnd = Utils.shiftDate(startTime, i + HALF_WINDOW_SIZE);
					SortedMap<Date, Double> items = bs.subMap(windowStart, windowEnd);

					if (!items.isEmpty())
					{
						Collection<Double> values = items.values();
						dataAvg.add(new DataPoint(windowMiddle, Utils.getSum(values) / (HALF_WINDOW_SIZE * 2)));
					}
				}

				LineGraphSeries<DataPoint> seriesAvg = new LineGraphSeries<>(dataAvg.toArray(new DataPoint[0]));
				seriesAvg.setColor(COLOR);

				return Collections.<Series<?>>singletonList(seriesAvg);
			}
		});
	}

	private void addChartX(int viewId)
	{
		Chart chart = (Chart) getChildFragmentManager().findFragmentById(viewId);
		if (chart == null)
		{
			chart = new Chart();
			getChildFragmentManager().beginTransaction().add(viewId, chart).commit();
		}

		// don't call getResources() on background thread
		final int COLOR = getResources().getColor(R.color.charts_x);

		chart.setChartType(ChartType.DAILY);
		chart.setTitle(String.format("%s, %s/%s", getString(R.string.common_rate_x), getString(R.string.common_unit_insulin),
				getString(R.string.common_unit_mass_gramm)));
		chart.setDescription(getString(R.string.common_rate_x_description) + ". " + getString(R.string.charts_type_daily) + ".");
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public Collection<Series<?>> load(Context context)
			{
				RateService rateService = RateServiceInternal.getInstance(context);

				List<DataPoint> dataList = new ArrayList<>();
				for (int time = 0; time <= Utils.MinPerDay; time += 30)
				{
					Rate rate = rateService.getRate(time);
					if (rate != null)
					{
						double x = (double) time / 60;
						double y = rate.getK() / rate.getQ();
						dataList.add(new DataPoint(x, y));
					}
				}

				DataPoint[] data = dataList.toArray(new DataPoint[0]);
				LineGraphSeries<DataPoint> series = new LineGraphSeries<>(data);
				series.setColor(COLOR);

				return Collections.<Series<?>>singletonList(series);
			}
		});
	}

	private void addChartK(int viewId)
	{
		Chart chart = (Chart) getChildFragmentManager().findFragmentById(viewId);
		if (chart == null)
		{
			chart = new Chart();
			getChildFragmentManager().beginTransaction().add(viewId, chart).commit();
		}

		// don't call getResources() on background thread
		final int COLOR = getResources().getColor(R.color.charts_k);

		chart.setChartType(ChartType.DAILY);
		chart.setTitle(String.format("%s, %s/%s", getString(R.string.common_rate_k), getBloodSugarUnitName(),
				getString(R.string.common_unit_mass_gramm)));
		chart.setDescription(getString(R.string.common_rate_k_description) + ". " + getString(R.string.charts_type_daily) + ".");
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public Collection<Series<?>> load(Context context)
			{
				RateService rateService = RateServiceInternal.getInstance(context);

				List<DataPoint> dataList = new ArrayList<>();
				for (int time = 0; time <= Utils.MinPerDay; time += 30)
				{
					Rate rate = rateService.getRate(time);
					if (rate != null)
					{
						double x = (double) time / 60;
						double y = BloodSugarUnit.convert(rate.getK(), BloodSugarUnit.MMOL_L, BLOOD_SUGAR_UNIT);
						dataList.add(new DataPoint(x, y));
					}
				}

				DataPoint[] data = dataList.toArray(new DataPoint[0]);
				LineGraphSeries<DataPoint> series = new LineGraphSeries<>(data);
				series.setColor(COLOR);

				return Collections.<Series<?>>singletonList(series);
			}
		});
	}

	private void addChartQ(int viewId)
	{
		Chart chart = (Chart) getChildFragmentManager().findFragmentById(viewId);
		if (chart == null)
		{
			chart = new Chart();
			getChildFragmentManager().beginTransaction().add(viewId, chart).commit();
		}

		// don't call getResources() on background thread
		final int COLOR = getResources().getColor(R.color.charts_q);

		chart.setChartType(ChartType.DAILY);
		chart.setTitle(String.format("%s, %s/%s", getString(R.string.common_rate_q), getBloodSugarUnitName(),
				getString(R.string.common_unit_insulin)));
		chart.setDescription(getString(R.string.common_rate_q_description) + ". " + getString(R.string.charts_type_daily) + ".");
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public Collection<Series<?>> load(Context context)
			{
				RateService rateService = RateServiceInternal.getInstance(context);

				List<DataPoint> dataList = new ArrayList<>();
				for (int time = 0; time <= Utils.MinPerDay; time += 30)
				{
					Rate rate = rateService.getRate(time);
					if (rate != null)
					{
						double x = (double) time / 60;
						double y = BloodSugarUnit.convert(rate.getQ(), BloodSugarUnit.MMOL_L, BLOOD_SUGAR_UNIT);
						dataList.add(new DataPoint(x, y));
					}
				}

				DataPoint[] data = dataList.toArray(new DataPoint[0]);
				LineGraphSeries<DataPoint> series = new LineGraphSeries<>(data);
				series.setColor(COLOR);

				return Collections.<Series<?>>singletonList(series);
			}
		});
	}
}