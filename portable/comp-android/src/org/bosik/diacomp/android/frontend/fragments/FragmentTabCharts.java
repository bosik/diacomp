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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.SortedMap;
import java.util.TreeMap;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.analyze.KoofServiceInternal;
import org.bosik.diacomp.android.backend.features.diary.LocalDiary;
import org.bosik.diacomp.android.frontend.views.Chart;
import org.bosik.diacomp.android.frontend.views.Chart.PostSetupListener;
import org.bosik.diacomp.android.frontend.views.ProgressBundle.DataLoader;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.services.analyze.KoofService;
import org.bosik.diacomp.core.services.analyze.entities.Koof;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;
import com.jjoe64.graphview.GraphView;
import com.jjoe64.graphview.LabelFormatter;
import com.jjoe64.graphview.Viewport;
import com.jjoe64.graphview.series.DataPoint;
import com.jjoe64.graphview.series.LineGraphSeries;
import com.jjoe64.graphview.series.Series;
import android.content.ContentResolver;
import android.graphics.Color;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

class HistoryLabels implements LabelFormatter
{
	private double maxY;

	public HistoryLabels(double maxY)
	{
		this.maxY = maxY;
	}

	@Override
	public String formatLabel(double value, boolean isValueX)
	{
		if (isValueX)
		{
			return new SimpleDateFormat("dd/MM", Locale.US).format(new Date((long) value));
		}
		else
		{
			if (maxY / 4 >= 1)
			{
				return String.format(Locale.US, "%.0f", value);
			}
			else if (maxY / 4 >= 0.1)
			{
				return String.format(Locale.US, "%.1f", value);
			}
			else
			{
				return String.format(Locale.US, "%.2f", value);
			}
		}
	}

	@Override
	public void setViewport(Viewport arg0)
	{
	}
}

class DailyLabels implements LabelFormatter
{
	private double maxY;

	public DailyLabels(double maxY)
	{
		this.maxY = maxY;
	}

	@Override
	public String formatLabel(double value, boolean isValueX)
	{
		if (isValueX)
		{
			return String.format(Locale.US, "%.0f", value);
		}
		else
		{
			if (maxY / 4 >= 1)
			{
				return String.format(Locale.US, "%.0f", value);
			}
			else if (maxY / 4 >= 0.1)
			{
				return String.format(Locale.US, "%.1f", value);
			}
			else
			{
				return String.format(Locale.US, "%.2f", value);
			}
		}
	}

	@Override
	public void setViewport(Viewport arg0)
	{
	}
}

class PostSetupHistory implements PostSetupListener
{
	@SuppressWarnings("rawtypes")
	protected static double getMaxY(List<Series> series)
	{
		Double result = null;
		for (Series<?> s : series)
		{
			if (result == null || s.getHighestValueY() > result)
			{
				result = s.getHighestValueY();
			}
		}
		return result != null ? result : 0.0;
	}

	@SuppressWarnings("rawtypes")
	protected static double getMinX(List<Series> series)
	{
		Double result = null;
		for (Series<?> s : series)
		{
			if (result == null || s.getLowestValueX() < result)
			{
				result = s.getLowestValueX();
			}
		}
		return result != null ? result : 0.0;
	}

	@SuppressWarnings("rawtypes")
	protected static double getMaxX(List<Series> series)
	{
		Double result = null;
		for (Series<?> s : series)
		{
			if (result == null || s.getHighestValueX() > result)
			{
				result = s.getHighestValueX();
			}
		}
		return result != null ? result : 0.0;
	}

	@Override
	public void onPostSetup(Chart chart)
	{
		final GraphView graphView = chart.getGraphView();

		graphView.getViewport().setXAxisBoundsManual(true);
		graphView.getViewport().setYAxisBoundsManual(true);
		graphView.getViewport().setMinY(0);

		double maxY;

		if (!graphView.getSeries().isEmpty())
		{
			maxY = FragmentTabCharts.addRoom(getMaxY(graphView.getSeries()));
			graphView.getViewport().setMaxY(maxY);
			graphView.getViewport().setMinX(getMinX(graphView.getSeries()));
			graphView.getViewport().setMaxX(getMaxX(graphView.getSeries()));
		}
		else
		{
			maxY = 1;
		}

		graphView.getGridLabelRenderer().setLabelFormatter(new HistoryLabels(maxY));
	}
}

public class FragmentTabCharts extends Fragment
{
	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
	{
		View rootView = inflater.inflate(R.layout.fragment_tab_charts, container, false);

		addChartBS();
		addChartInsulinConsumption();

		addChartX();
		addChartK();
		addChartQ();

		return rootView;
	}

	public static double addRoom(double max)
	{
		double top = 0.04;

		while (top < max)
		{
			if (top * 2 > max)
			{
				return top * 2;
			}
			if (top * 5 > max)
			{
				return top * 5;
			}
			top *= 10;
		}

		return top;
	}

	private void addChartBS()
	{
		Chart chart = (Chart) getChildFragmentManager().findFragmentById(R.id.chartBS);
		if (chart == null)
		{
			chart = new Chart();
			getChildFragmentManager().beginTransaction().add(R.id.chartBS, chart).commit();
		}

		chart.setTitle(String.format("%s, %s", getString(R.string.charts_average_bs),
				getString(R.string.common_unit_bs_mmoll)));
		chart.setDescription(getString(R.string.charts_average_bs_description));
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public Collection<Series<?>> load(ContentResolver contentResolver)
			{
				final int WINDOW_SIZE = 7; // days
				final int PERIOD = 30; // days

				// PREPARE DATE TREE

				DiaryService diary = LocalDiary.getInstance(contentResolver);
				Date endTime = new Date();
				Date startTime = Utils.shiftDate(endTime, -PERIOD - WINDOW_SIZE);
				List<Versioned<DiaryRecord>> recs = diary.findPeriod(startTime, endTime, false);

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

				List<DataPoint> dataAvg = new ArrayList<DataPoint>();
				List<DataPoint> dataMin = new ArrayList<DataPoint>();
				List<DataPoint> dataMax = new ArrayList<DataPoint>();

				for (int i = 0; i < PERIOD; i++)
				{
					Date windowStart = Utils.shiftDate(startTime, i);
					Date windowEnd = Utils.shiftDate(startTime, i + WINDOW_SIZE);
					SortedMap<Date, Double> items = bs.subMap(windowStart, windowEnd);

					if (!items.isEmpty())
					{
						Collection<Double> values = items.values();

						double mean = Utils.getMean(values);
						double deviation = Utils.getDeviation(values, mean);
						dataAvg.add(new DataPoint(windowEnd, mean));
						dataMin.add(new DataPoint(windowEnd, mean - deviation));
						dataMax.add(new DataPoint(windowEnd, mean + deviation));
					}
				}

				LineGraphSeries<DataPoint> seriesAvg = new LineGraphSeries<DataPoint>(
						dataAvg.toArray(new DataPoint[dataAvg.size()]));
				seriesAvg.setColor(Color.rgb(255, 0, 0));

				LineGraphSeries<DataPoint> seriesMin = new LineGraphSeries<DataPoint>(
						dataMin.toArray(new DataPoint[dataMin.size()]));
				seriesMin.setColor(Color.rgb(255, 224, 224));

				LineGraphSeries<DataPoint> seriesMax = new LineGraphSeries<DataPoint>(
						dataMax.toArray(new DataPoint[dataMax.size()]));
				seriesMax.setColor(Color.rgb(255, 224, 224));

				return Arrays.<Series<?>> asList(seriesMin, seriesAvg, seriesMax);
			}
		});
		chart.setPostSetupListener(new PostSetupHistory());
	}

	private void addChartInsulinConsumption()
	{
		Chart chart = (Chart) getChildFragmentManager().findFragmentById(R.id.chartInsulin);
		if (chart == null)
		{
			chart = new Chart();
			getChildFragmentManager().beginTransaction().add(R.id.chartInsulin, chart).commit();
		}

		chart.setTitle(String.format("%s, %s/%s", getString(R.string.common_koof_x),
				getString(R.string.common_unit_insulin), getString(R.string.common_unit_mass_gramm)));
		chart.setDescription(getString(R.string.charts_insulin_consumption_history_description));
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public Collection<Series<?>> load(ContentResolver contentResolver)
			{
				final int WINDOW_SIZE = 7; // days
				final int PERIOD = 30; // days

				// PREPARE DATE TREE

				DiaryService diary = LocalDiary.getInstance(contentResolver);
				Date endTime = new Date();
				Date startTime = Utils.shiftDate(endTime, -PERIOD - WINDOW_SIZE);
				List<Versioned<DiaryRecord>> recs = diary.findPeriod(startTime, endTime, false);

				SortedMap<Date, DiaryRecord> bs = new TreeMap<>();
				for (Versioned<DiaryRecord> rec : recs)
				{
					bs.put(rec.getData().getTime(), rec.getData());
				}

				// ANALYZE & FILL SERIES

				List<DataPoint> data = new ArrayList<DataPoint>();

				for (int i = 0; i < PERIOD; i++)
				{
					Date windowStart = Utils.shiftDate(startTime, i);
					Date windowEnd = Utils.shiftDate(startTime, i + WINDOW_SIZE);
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
							data.add(new DataPoint(windowEnd, summInsulin / summCarbs));
						}
					}
				}

				LineGraphSeries<DataPoint> series = new LineGraphSeries<DataPoint>(
						data.toArray(new DataPoint[data.size()]));
				series.setColor(Color.rgb(192, 192, 255));

				return Arrays.<Series<?>> asList(series);
			}
		});
		chart.setPostSetupListener(new PostSetupHistory());
	}

	private void addChartX()
	{
		Chart chart = (Chart) getChildFragmentManager().findFragmentById(R.id.chartX);
		if (chart == null)
		{
			chart = new Chart();
			getChildFragmentManager().beginTransaction().add(R.id.chartX, chart).commit();
		}

		chart.setTitle(String.format("%s, %s/%s", getString(R.string.common_koof_x),
				getString(R.string.common_unit_insulin), getString(R.string.common_unit_mass_gramm)));
		chart.setDescription(getString(R.string.charts_insulin_consumption_daily_description));
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public Collection<Series<?>> load(ContentResolver contentResolver)
			{
				KoofService koofService = KoofServiceInternal.getInstance(contentResolver);

				List<DataPoint> dataList = new ArrayList<DataPoint>();
				for (int time = 0; time <= Utils.MinPerDay; time += 30)
				{
					Koof koof = koofService.getKoof(time);
					double x = (double) time / 60;
					double y = koof.getK() / koof.getQ();
					dataList.add(new DataPoint(x, y));
				}
				DataPoint[] data = dataList.toArray(new DataPoint[dataList.size()]);
				LineGraphSeries<DataPoint> series = new LineGraphSeries<DataPoint>(data);
				series.setColor(Color.rgb(192, 192, 192));

				return Arrays.<Series<?>> asList(series);
			}
		});
		chart.setPostSetupListener(new PostSetupListener()
		{
			@Override
			public void onPostSetup(Chart chart)
			{
				chart.getGraphView().getViewport().setXAxisBoundsManual(true);
				chart.getGraphView().getViewport().setYAxisBoundsManual(true);
				chart.getGraphView().getViewport().setMinX(0);
				chart.getGraphView().getViewport().setMaxX(24);
				chart.getGraphView().getViewport().setMinY(0);

				if (!chart.getGraphView().getSeries().isEmpty())
				{
					double maxY = addRoom(chart.getGraphView().getSeries().get(0).getHighestValueY());
					chart.getGraphView().getViewport().setMaxY(maxY);
					chart.getGraphView().getGridLabelRenderer().setLabelFormatter(new DailyLabels(maxY));
				}
			}
		});
	}

	private void addChartK()
	{
		Chart chart = (Chart) getChildFragmentManager().findFragmentById(R.id.chartK);
		if (chart == null)
		{
			chart = new Chart();
			getChildFragmentManager().beginTransaction().add(R.id.chartK, chart).commit();
		}

		chart.setTitle(String.format("%s, %s/%s", getString(R.string.common_koof_k),
				getString(R.string.common_unit_bs_mmoll), getString(R.string.common_unit_mass_gramm)));
		chart.setDescription(getString(R.string.charts_koof_k_daily_description));
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public Collection<Series<?>> load(ContentResolver contentResolver)
			{
				KoofService koofService = KoofServiceInternal.getInstance(contentResolver);

				List<DataPoint> dataList = new ArrayList<DataPoint>();
				for (int time = 0; time <= Utils.MinPerDay; time += 30)
				{
					double x = (double) time / 60;
					double y = koofService.getKoof(time).getK();
					dataList.add(new DataPoint(x, y));
				}
				DataPoint[] data = dataList.toArray(new DataPoint[dataList.size()]);
				LineGraphSeries<DataPoint> series = new LineGraphSeries<DataPoint>(data);
				series.setColor(Color.rgb(255, 0, 0));

				return Arrays.<Series<?>> asList(series);
			}
		});
		chart.setPostSetupListener(new PostSetupListener()
		{
			@Override
			public void onPostSetup(Chart chart)
			{
				chart.getGraphView().getViewport().setXAxisBoundsManual(true);
				chart.getGraphView().getViewport().setYAxisBoundsManual(true);
				chart.getGraphView().getViewport().setMinX(0);
				chart.getGraphView().getViewport().setMaxX(24);
				chart.getGraphView().getViewport().setMinY(0);

				if (!chart.getGraphView().getSeries().isEmpty())
				{
					double maxY = addRoom(chart.getGraphView().getSeries().get(0).getHighestValueY());
					chart.getGraphView().getViewport().setMaxY(maxY);
					chart.getGraphView().getGridLabelRenderer().setLabelFormatter(new DailyLabels(maxY));
				}
			}
		});
	}

	private void addChartQ()
	{
		Chart chart = (Chart) getChildFragmentManager().findFragmentById(R.id.chartQ);
		if (chart == null)
		{
			chart = new Chart();
			getChildFragmentManager().beginTransaction().add(R.id.chartQ, chart).commit();
		}

		chart.setTitle(String.format("%s, %s/%s", getString(R.string.common_koof_q),
				getString(R.string.common_unit_bs_mmoll), getString(R.string.common_unit_insulin)));
		chart.setDescription(getString(R.string.charts_koof_q_daily_description));
		chart.setDataLoader(new DataLoader()
		{
			@Override
			public Collection<Series<?>> load(ContentResolver contentResolver)
			{
				KoofService koofService = KoofServiceInternal.getInstance(contentResolver);

				List<DataPoint> dataList = new ArrayList<DataPoint>();
				for (int time = 0; time <= Utils.MinPerDay; time += 30)
				{
					double x = (double) time / 60;
					double y = koofService.getKoof(time).getQ();
					dataList.add(new DataPoint(x, y));
				}
				DataPoint[] data = dataList.toArray(new DataPoint[dataList.size()]);
				LineGraphSeries<DataPoint> series = new LineGraphSeries<DataPoint>(data);
				series.setColor(Color.rgb(0, 0, 255));

				return Arrays.<Series<?>> asList(series);
			}
		});
		chart.setPostSetupListener(new PostSetupListener()
		{
			@Override
			public void onPostSetup(Chart chart)
			{
				final GraphView graphView = chart.getGraphView();

				graphView.getViewport().setXAxisBoundsManual(true);
				graphView.getViewport().setYAxisBoundsManual(true);
				graphView.getViewport().setMinX(0);
				graphView.getViewport().setMaxX(24);
				graphView.getViewport().setMinY(0);

				if (!graphView.getSeries().isEmpty())
				{
					double maxY = addRoom(chart.getGraphView().getSeries().get(0).getHighestValueY());
					chart.getGraphView().getViewport().setMaxY(maxY);
					chart.getGraphView().getGridLabelRenderer().setLabelFormatter(new DailyLabels(maxY));
				}
			}
		});
	}
}