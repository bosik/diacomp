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

import android.content.ContentResolver;
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
import org.bosik.diacomp.android.backend.features.analyze.KoofServiceInternal;
import org.bosik.diacomp.android.backend.features.diary.LocalDiary;
import org.bosik.diacomp.android.frontend.fragments.chart.Chart;
import org.bosik.diacomp.android.frontend.fragments.chart.Chart.ChartType;
import org.bosik.diacomp.android.frontend.fragments.chart.ProgressBundle.DataLoader;
import org.bosik.diacomp.android.frontend.views.expandable.ExpandableView;
import org.bosik.diacomp.android.frontend.views.expandable.OnSwitchedListener;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.services.analyze.KoofService;
import org.bosik.diacomp.core.services.analyze.entities.Koof;
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

		ExpandableView groupHistory = (ExpandableView) rootView.findViewById(R.id.chartGroupHistoryTitle);
		LinearLayout groupHistoryContent = (LinearLayout) rootView.findViewById(R.id.chartGroupHistoryContent);
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

		ExpandableView groupDaily = (ExpandableView) rootView.findViewById(R.id.chartGroupDailyTitle);
		LinearLayout groupDailyContent = (LinearLayout) rootView.findViewById(R.id.chartGroupDailyContent);
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

		if (AccountUtils.getAccounts(getActivity()).length > 0)
		{
			MenuItem item = menu.findItem(R.id.item_common_login);
			if (item != null)
			{
				item.setVisible(false);
			}
		}
	}

	private static final int HALF_WINDOW_SIZE = 2;    // days
	private static final int PERIOD           = 30;    // days

	private void addChartBS(int viewId)
	{
		Chart chart = (Chart) getChildFragmentManager().findFragmentById(viewId);

		if (chart == null)
		{
			chart = new Chart();
			getChildFragmentManager().beginTransaction().add(viewId, chart).commit();
		}

		chart.setChartType(ChartType.HISTORY);
		chart.setTitle(String.format("%s, %s", getString(R.string.charts_average_bs), getString(R.string.common_unit_bs_mmoll)));
		chart.setDescription(getString(R.string.charts_average_bs_description));
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public Collection<Series<?>> load(ContentResolver contentResolver)
			{
				// PREPARE DATE TREE

				DiaryService diary = LocalDiary.getInstance(FragmentTabCharts.this.getActivity());
				Date startTime = Utils.shiftDate(new Date(), -PERIOD - HALF_WINDOW_SIZE);
				Date endTime = Utils.shiftDate(new Date(), -HALF_WINDOW_SIZE);
				List<Versioned<DiaryRecord>> recs = diary
						.findPeriod(Utils.shiftDate(startTime, -HALF_WINDOW_SIZE), Utils.shiftDate(endTime, HALF_WINDOW_SIZE), false);

				SortedMap<Date, Double> bs = new TreeMap<>();
				for (Versioned<DiaryRecord> rec : recs)
				{
					if (rec.getData() instanceof BloodRecord)
					{
						BloodRecord blood = (BloodRecord) rec.getData();
						bs.put(blood.getTime(), blood.getValue());
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
						Collection<Double> values = items.values();

						double mean = Utils.getMean(values);
						double deviation = Utils.getDeviation(values, mean);
						dataAvg.add(new DataPoint(windowMiddle, mean));
						dataMin.add(new DataPoint(windowMiddle, mean - deviation));
						dataMax.add(new DataPoint(windowMiddle, mean + deviation));
					}
				}

				LineGraphSeries<DataPoint> seriesAvg = new LineGraphSeries<>(dataAvg.toArray(new DataPoint[dataAvg.size()]));
				seriesAvg.setColor(getResources().getColor(R.color.charts_bs_average));

				LineGraphSeries<DataPoint> seriesMin = new LineGraphSeries<>(dataMin.toArray(new DataPoint[dataMin.size()]));
				seriesMin.setColor(getResources().getColor(R.color.charts_bs_dispersion));

				LineGraphSeries<DataPoint> seriesMax = new LineGraphSeries<>(dataMax.toArray(new DataPoint[dataMax.size()]));
				seriesMax.setColor(getResources().getColor(R.color.charts_bs_dispersion));

				return Arrays.<Series<?>>asList(seriesMin, seriesAvg, seriesMax);
			}
		});
	}

	private void addChartInsulinConsumption(int viewId)
	{
		Chart chart = (Chart) getChildFragmentManager().findFragmentById(viewId);
		if (chart == null)
		{
			chart = new Chart();
			getChildFragmentManager().beginTransaction().add(viewId, chart).commit();
		}

		chart.setChartType(ChartType.HISTORY);
		chart.setTitle(String.format("%s, %s/%s", getString(R.string.common_koof_x), getString(R.string.common_unit_insulin),
				getString(R.string.common_unit_mass_gramm)));
		chart.setDescription(getString(R.string.charts_insulin_consumption_history_description));
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public Collection<Series<?>> load(ContentResolver contentResolver)
			{
				// PREPARE DATE TREE

				DiaryService diary = LocalDiary.getInstance(FragmentTabCharts.this.getActivity());
				Date startTime = Utils.shiftDate(new Date(), -PERIOD - HALF_WINDOW_SIZE);
				Date endTime = Utils.shiftDate(new Date(), -HALF_WINDOW_SIZE);
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

						double summCarbs = 0.0;
						double summInsulin = 0.0;

						for (DiaryRecord rec : values)
						{
							if (rec instanceof MealRecord)
							{
								summCarbs += ((MealRecord) rec).getCarbs();
							}
							else if (rec instanceof InsRecord)
							{
								summInsulin += ((InsRecord) rec).getValue();
							}
						}

						if (summCarbs > Utils.EPS)
						{
							data.add(new DataPoint(windowMiddle, summInsulin / summCarbs));
						}
					}
				}

				LineGraphSeries<DataPoint> series = new LineGraphSeries<>(data.toArray(new DataPoint[data.size()]));
				series.setColor(getResources().getColor(R.color.charts_insulin_consumption));

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

		chart.setChartType(ChartType.HISTORY);
		chart.setTitle(String.format("%s, %s", getString(R.string.charts_average_calories), getString(R.string.common_unit_value_kcal)));
		chart.setDescription(getString(R.string.charts_average_calories_description));
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public Collection<Series<?>> load(ContentResolver contentResolver)
			{
				// PREPARE DATE TREE

				DiaryService diary = LocalDiary.getInstance(FragmentTabCharts.this.getActivity());
				Date startTime = Utils.shiftDate(new Date(), -PERIOD - HALF_WINDOW_SIZE);
				Date endTime = Utils.shiftDate(new Date(), -HALF_WINDOW_SIZE);
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
						double summ = Utils.getSumm(values);
						dataAvg.add(new DataPoint(windowMiddle, summ / (HALF_WINDOW_SIZE * 2)));
					}
				}

				LineGraphSeries<DataPoint> seriesAvg = new LineGraphSeries<>(dataAvg.toArray(new DataPoint[dataAvg.size()]));
				seriesAvg.setColor(getResources().getColor(R.color.charts_value));

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

		chart.setChartType(ChartType.DAILY);
		chart.setTitle(String.format("%s, %s/%s", getString(R.string.common_koof_x), getString(R.string.common_unit_insulin),
				getString(R.string.common_unit_mass_gramm)));
		chart.setDescription(getString(R.string.charts_insulin_consumption_daily_description));
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public Collection<Series<?>> load(ContentResolver contentResolver)
			{
				KoofService koofService = KoofServiceInternal.getInstance(getActivity());

				List<DataPoint> dataList = new ArrayList<>();
				for (int time = 0; time <= Utils.MinPerDay; time += 30)
				{
					Koof koof = koofService.getKoof(time);
					double x = (double) time / 60;
					double y = koof.getK() / koof.getQ();
					dataList.add(new DataPoint(x, y));
				}
				DataPoint[] data = dataList.toArray(new DataPoint[dataList.size()]);
				LineGraphSeries<DataPoint> series = new LineGraphSeries<>(data);
				series.setColor(getResources().getColor(R.color.charts_x));

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

		chart.setChartType(ChartType.DAILY);
		chart.setTitle(String.format("%s, %s/%s", getString(R.string.common_koof_k), getString(R.string.common_unit_bs_mmoll),
				getString(R.string.common_unit_mass_gramm)));
		chart.setDescription(getString(R.string.charts_koof_k_daily_description));
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public Collection<Series<?>> load(ContentResolver contentResolver)
			{
				KoofService koofService = KoofServiceInternal.getInstance(getActivity());

				List<DataPoint> dataList = new ArrayList<>();
				for (int time = 0; time <= Utils.MinPerDay; time += 30)
				{
					double x = (double) time / 60;
					double y = koofService.getKoof(time).getK();
					dataList.add(new DataPoint(x, y));
				}
				DataPoint[] data = dataList.toArray(new DataPoint[dataList.size()]);
				LineGraphSeries<DataPoint> series = new LineGraphSeries<>(data);
				series.setColor(getResources().getColor(R.color.charts_k));

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

		chart.setChartType(ChartType.DAILY);
		chart.setTitle(String.format("%s, %s/%s", getString(R.string.common_koof_q), getString(R.string.common_unit_bs_mmoll),
				getString(R.string.common_unit_insulin)));
		chart.setDescription(getString(R.string.charts_koof_q_daily_description));
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public Collection<Series<?>> load(ContentResolver contentResolver)
			{
				KoofService koofService = KoofServiceInternal.getInstance(getActivity());

				List<DataPoint> dataList = new ArrayList<>();
				for (int time = 0; time <= Utils.MinPerDay; time += 30)
				{
					double x = (double) time / 60;
					double y = koofService.getKoof(time).getQ();
					dataList.add(new DataPoint(x, y));
				}
				DataPoint[] data = dataList.toArray(new DataPoint[dataList.size()]);
				LineGraphSeries<DataPoint> series = new LineGraphSeries<>(data);
				series.setColor(getResources().getColor(R.color.charts_q));

				return Collections.<Series<?>>singletonList(series);
			}
		});
	}
}