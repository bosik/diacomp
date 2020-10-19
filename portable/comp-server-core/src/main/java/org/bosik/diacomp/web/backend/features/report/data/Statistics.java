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
package org.bosik.diacomp.web.backend.features.report.data;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.function.Function;

import static java.util.stream.Collectors.toList;

@Data
public class Statistics
{
	private final List<Versioned<DiaryRecord>>              records;
	@JsonIgnore
	private final Map<String, List<Versioned<DiaryRecord>>> recordsPerDay;
	private final Map<String, Metrics>                      metrics;
	private final Metrics                                   totalMetrics;
	@JsonIgnore
	private final AverageBS                                 averageBS;
	private final double                                    targetMinBS;
	private final double                                    targetMaxBS;
	private final Double                                    targetAchievement; // 0..1
	private final String                                    dateStart;
	private final String                                    dateEnd;
	private final TimeZone                                  timeZone;

	public Statistics(List<Versioned<DiaryRecord>> records, Date fromDate, Date toDate, double targetMinBS,
			double targetMaxBS, TimeZone timeZone)
	{
		this.records = records;
		this.recordsPerDay = groupByDate(records, timeZone);
		this.metrics = buildMetrics(recordsPerDay, timeZone);
		this.totalMetrics = new Metrics(records, timeZone);
		this.averageBS = new AverageBS(records, 20);
		this.targetMinBS = targetMinBS;
		this.targetMaxBS = targetMaxBS;
		this.targetAchievement = calculateTargetAchievement(records, targetMinBS, targetMaxBS);
		this.dateStart = formatDate(records.stream().map(e -> e.getData().getTime()).min(Date::compareTo).orElse(fromDate), timeZone);
		this.dateEnd = formatDate(records.stream().map(e -> e.getData().getTime()).max(Date::compareTo).orElse(toDate), timeZone);
		this.timeZone = timeZone;
	}

	private static Map<String, List<Versioned<DiaryRecord>>> groupByDate(List<Versioned<DiaryRecord>> records, TimeZone timeZone)
	{
		final Map<String, List<Versioned<DiaryRecord>>> paged = new LinkedHashMap<>();
		for (Versioned<DiaryRecord> item : records)
		{
			final String date = getDate(item, timeZone);
			paged.putIfAbsent(date, new ArrayList<>());
			paged.get(date).add(item);
		}

		return paged;
	}

	private static Map<String, Metrics> buildMetrics(Map<String, List<Versioned<DiaryRecord>>> diary, TimeZone timeZone)
	{
		final Map<String, Metrics> metricsMap = new LinkedHashMap<>();
		diary.forEach((date, records) -> metricsMap.put(date, new Metrics(records, timeZone)));
		return metricsMap;
	}

	private Double calculateTargetAchievement(List<Versioned<DiaryRecord>> records, double targetMinBS, double targetMaxBS)
	{
		final List<BloodRecord> bloodRecords = filterRecords(records, BloodRecord.class);

		if (bloodRecords.isEmpty())
		{
			return null;
		}

		double timeTotal = 0.0;
		double timeTarget = 0.0;

		for (int i = 0; i < bloodRecords.size() - 1; i++)
		{
			double t1 = bloodRecords.get(i).getTime().getTime();
			double t2 = bloodRecords.get(i + 1).getTime().getTime();
			double v1 = bloodRecords.get(i).getValue();
			double v2 = bloodRecords.get(i + 1).getValue();

			timeTotal += (t2 - t1);
			timeTarget += getIntersectionPeriod(t1, v1, t2, v2, targetMinBS, targetMaxBS);
		}

		return timeTarget / timeTotal;
	}

	/**
	 * Given BS {@code v1} at moment {@code t1}, BS {@code v2} at moment {@code t2} and assuming linear interpolation,
	 * how long BS was in [{@code minV}, {@code maxV}) range?
	 * Examples:
	 * <ul>
	 * <li>if {@code v1} in ({@code minV}, {@code maxV}), {@code v2} in ({@code minV}, {@code maxV}), result is {@code t2} - {@code t1}</li>
	 * <li>if {@code v1} > {@code maxV}, {@code v2} > {@code maxV}, result is 0</li>
	 * </ul>
	 *
	 * @param t1
	 * @param v1
	 * @param t2
	 * @param v2
	 * @param minV
	 * @param maxV
	 * @return Intersection period
	 */
	private static double getIntersectionPeriod(double t1, double v1, double t2, double v2, double minV, double maxV)
	{
		if (t1 >= t2)
		{
			throw new IllegalArgumentException("t1 (" + t1 + ") must be less than t2 (" + t2 + ")");
		}

		if (minV >= maxV)
		{
			throw new IllegalArgumentException("minV (" + minV + ") must be less than maxV (" + maxV + ")");
		}

		if (v1 < minV)
		{
			if (v2 < minV)
			{
				return 0.0;
			}
			else if (v2 < maxV)
			{
				return t2 - getIntersectionPoint(t1, v1, t2, v2, minV);
			}
			else
			{
				return getIntersectionPoint(t1, v1, t2, v2, maxV) - getIntersectionPoint(t1, v1, t2, v2, minV);
			}
		}
		else if (v1 < maxV)
		{
			if (v2 < minV)
			{
				return getIntersectionPoint(t1, v1, t2, v2, minV) - t1;
			}
			else if (v2 < maxV)
			{
				return t2 - t1;
			}
			else
			{
				return getIntersectionPoint(t1, v1, t2, v2, maxV) - t1;
			}
		}
		else
		{
			if (v2 < minV)
			{
				return getIntersectionPoint(t1, v1, t2, v2, minV) - getIntersectionPoint(t1, v1, t2, v2, maxV);
			}
			else if (v2 < maxV)
			{
				return t2 - getIntersectionPoint(t1, v1, t2, v2, maxV);
			}
			else
			{
				return 0.0;
			}
		}
	}

	/**
	 * Calculates abscissa of intersection point of line ((t1, v1), (t2, v2)) and line y = value
	 *
	 * @param t1
	 * @param v1
	 * @param t2
	 * @param v2
	 * @param value
	 * @return
	 */
	private static double getIntersectionPoint(double t1, double v1, double t2, double v2, double value)
	{
		return t1 + (value - v1) * (t2 - t1) / (v2 - v1);
	}

	// ==== UTILS ===========================

	public static <T extends DiaryRecord> List<T> filterRecords(List<Versioned<DiaryRecord>> records, Class<T> type)
	{
		return records.stream()
				.filter(r -> type.isAssignableFrom(r.getData().getClass()))
				.map(r -> type.cast(r.getData()))
				.collect(toList());
	}

	public static String getDate(Versioned<DiaryRecord> item, TimeZone timeZone)
	{
		return formatDate(item.getData().getTime(), timeZone);
	}

	private static String formatDate(Date date, TimeZone timeZone)
	{
		return Utils.formatDateLocal(timeZone, date);
	}

	public static <T> double weightedAverage(Iterable<T> data, Function<T, Double> valueFunction, Function<T, Double> weightFunction)
	{
		double totalSum = 0.0;
		double totalWeight = 0.0;

		for (T item : data)
		{
			final double value = valueFunction.apply(item);
			final double weight = weightFunction.apply(item);
			totalSum += weight * value;
			totalWeight += weight;
		}

		return totalSum / totalWeight;
	}
}
