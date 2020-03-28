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

import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.services.analyze.entities.TimePoint;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

import java.util.List;

import static java.util.stream.Collectors.toList;

public class AverageBS
{
	private final int      discreteness;
	private final double[] means;
	private final double[] deviations;

	/**
	 * @param records      Data to analyze
	 * @param discreteness in minutes
	 */
	public AverageBS(List<Versioned<DiaryRecord>> records, int discreteness)
	{
		if (discreteness < 1 || discreteness > 4 * Utils.MsecPerHour)
		{
			throw new IllegalArgumentException("Discreteness must be between 1 and " + 4 * Utils.MsecPerHour);
		}

		final List<TimePoint> input = Statistics.filterRecords(records, BloodRecord.class).stream()
				.map(r -> new TimePoint(Utils.getDayMinutesLocal(r.getTime()), r.getValue()))
				.collect(toList());

		this.discreteness = discreteness;

		if (input.isEmpty())
		{
			this.means = null;
			this.deviations = null;
			return;
		}

		final int count = Utils.MinPerDay / discreteness;
		this.means = new double[count];
		this.deviations = new double[count];

		for (int i = 0; i < count; i++)
		{
			int time = i * discreteness;

			final double average = Statistics.weightedAverage(
					input,
					item -> item.getValue(),
					item -> getTimeWeight(time - item.getTime())
			);

			means[i] = average;
			deviations[i] = Math.sqrt(Statistics.weightedAverage(
					input,
					item -> (item.getValue() - average) * (item.getValue() - average),
					item -> getTimeWeight(time - item.getTime())
			));
		}
	}

	public double getMean(int time)
	{
		final int t1 = time / discreteness % means.length;
		final int t2 = (time / discreteness + 1) % means.length;
		final double k = (time % discreteness) / (double) discreteness;

		return means[t1] * (1 - k) + means[t2] * k;
	}

	public double getDeviation(int time)
	{
		final int t1 = time / discreteness % means.length;
		final int t2 = (time / discreteness + 1) % means.length;
		final double k = (time % discreteness) / (double) discreteness;

		return deviations[t1] * (1 - k) + deviations[t2] * k;
	}

	private static double getTimeWeight(int deltaTime)
	{
		deltaTime = Math.abs(deltaTime);
		deltaTime = Math.min(deltaTime, Utils.MinPerDay - deltaTime);
		return Math.exp(-0.00005 * deltaTime * deltaTime);
	}

	public boolean isEmpty()
	{
		return means == null || deviations == null;
	}
}
