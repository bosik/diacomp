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

import lombok.Value;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.merklesync.Versioned;

import java.util.List;
import java.util.TimeZone;

@Value
public class Metrics
{
	private int    count;
	private double totalProts;
	private double totalFats;
	private double totalCarbs;
	private double totalValue;
	private double totalIns;
	private Double averageProts;
	private Double averageFats;
	private Double averageCarbs;
	private Double averageValue;
	private Double averageIns;
	private int    totalBsCount;
	private Double averageBs;
	private Double deviationBs;

	public Metrics(List<Versioned<DiaryRecord>> records, TimeZone timeZone)
	{
		final List<BloodRecord> bloodRecords = Statistics.filterRecords(records, BloodRecord.class);
		final List<InsRecord> insRecords = Statistics.filterRecords(records, InsRecord.class);
		final List<MealRecord> meals = Statistics.filterRecords(records, MealRecord.class);

		this.count = countDistinctDays(records, timeZone);
		this.totalBsCount = bloodRecords.size();

		if (bloodRecords.size() > 0)
		{
			this.averageBs = bloodRecords.stream().mapToDouble(BloodRecord::getValue).average().getAsDouble();
			this.deviationBs = Math.sqrt(bloodRecords.stream()
					.mapToDouble(r -> r.getValue() - averageBs)
					.map(x -> x * x)
					.average()
					.getAsDouble()
			);
		}
		else
		{
			this.averageBs = null;
			this.deviationBs = null;
		}

		this.totalIns = insRecords.stream().mapToDouble(InsRecord::getValue).sum();
		this.totalProts = meals.stream().mapToDouble(MealRecord::getProts).sum();
		this.totalFats = meals.stream().mapToDouble(MealRecord::getFats).sum();
		this.totalCarbs = meals.stream().mapToDouble(MealRecord::getCarbs).sum();
		this.totalValue = meals.stream().mapToDouble(MealRecord::getValue).sum();

		this.averageIns = count > 0 ? totalIns / count : null;
		this.averageProts = count > 0 ? totalProts / count : null;
		this.averageFats = count > 0 ? totalFats / count : null;
		this.averageCarbs = count > 0 ? totalCarbs / count : null;
		this.averageValue = count > 0 ? totalValue / count : null;
	}

	private int countDistinctDays(List<Versioned<DiaryRecord>> records, TimeZone timeZone)
	{
		return (int) records.stream()
				.map(e -> Statistics.getDate(e, timeZone))
				.distinct()
				.count();
	}
}
