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
package org.bosik.diacomp.core.services.diary;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

public class PostprandUtils
{
	// TODO: move hardcode
	private static final int	DEFAULT_AFFECT_TIME_INSULIN		= 210;
	private static final int	DEFAULT_AFFECT_TIME_MEAL_STD	= 210;
	private static final int	DEFAULT_AFFECT_TIME_MEAL_SHORT	= 20;

	/**
	 * 
	 * @param records
	 * @param insulinAffectTime
	 *            (minutes)
	 * @param mealAffectTime
	 *            (minutes)
	 * @param mealShortAffectTime
	 *            (minutes)
	 */
	public static void updatePostprand(List<Versioned<DiaryRecord>> records, int insulinAffectTime, int mealAffectTime,
			int mealShortAffectTime)
	{
		long minFreeTime = 0;

		for (Versioned<DiaryRecord> versionedRecord : records)
		{
			DiaryRecord record = versionedRecord.getData();
			if (record instanceof InsRecord)
			{
				long curFreeTime = record.getTime().getTime() + insulinAffectTime * Utils.MsecPerMin;
				if (curFreeTime > minFreeTime)
				{
					minFreeTime = curFreeTime;
				}
			}
			else if (record instanceof MealRecord)
			{
				long affectTime = ((MealRecord)record).getShortMeal() ? mealShortAffectTime : mealAffectTime;
				long curFreeTime = record.getTime().getTime() + affectTime * Utils.MsecPerMin;
				if (curFreeTime > minFreeTime)
				{
					minFreeTime = curFreeTime;
				}
			}
			else if (record instanceof BloodRecord)
			{
				((BloodRecord)record).setPostPrand(record.getTime().getTime() < minFreeTime);
			}
		}
	}

	/**
	 * Deprecated. Use {@link #updatePostprand(List, int, int, int)} instead and pass the params directly.
	 * 
	 * @param records
	 */
	@Deprecated
	public static void updatePostprand(List<Versioned<DiaryRecord>> records)
	{
		updatePostprand(records, DEFAULT_AFFECT_TIME_INSULIN, DEFAULT_AFFECT_TIME_MEAL_STD,
				DEFAULT_AFFECT_TIME_MEAL_SHORT);
	}

	/**
	 * Searches for the insulin injection nearest to the specified time
	 * 
	 * @param near
	 *            Time to search around
	 * @param scanPeriod
	 *            , in seconds
	 * @return
	 */
	public static InsRecord findNearestInsulin(DiaryService diary, Date near, long scanPeriod)
	{
		Date startTime = new Date(near.getTime() - (scanPeriod * Utils.MsecPerSec));
		Date endTime = new Date(near.getTime() + (scanPeriod * Utils.MsecPerSec));
		List<Versioned<DiaryRecord>> records = diary.findPeriod(startTime, endTime, false);

		long min = scanPeriod * Utils.MsecPerSec * 2;
		InsRecord nearestIns = null;

		for (Versioned<DiaryRecord> record : records)
		{
			if (record.getData() instanceof InsRecord)
			{
				InsRecord currentIns = (InsRecord)record.getData();
				long currentDist = Math.abs(currentIns.getTime().getTime() - near.getTime());
				if (currentDist < min)
				{
					nearestIns = currentIns;
					min = currentDist;
				}
			}
		}

		return nearestIns;
	}

	/**
	 * Searches for the last BS record in period [since - scanPeriod, since]
	 * 
	 * @param since
	 * @param scanPeriod
	 *            , in seconds
	 * 
	 * @return
	 */
	public static BloodRecord findLastBlood(DiaryService diary, Date since, long scanPeriod, boolean skipPostprandials)
	{
		Date endTime = since;
		Date startTime = new Date(endTime.getTime() - (scanPeriod * Utils.MsecPerSec));

		List<Versioned<DiaryRecord>> records = diary.findPeriod(startTime, endTime, false);
		updatePostprand(records);
		Collections.reverse(records);

		for (Versioned<DiaryRecord> record : records)
		{
			if (record.getData() instanceof BloodRecord)
			{
				BloodRecord bloodRecord = (BloodRecord)record.getData();
				if (!skipPostprandials || !bloodRecord.isPostPrand())
				{
					return bloodRecord;
				}
			}
		}

		return null;
	}
}
