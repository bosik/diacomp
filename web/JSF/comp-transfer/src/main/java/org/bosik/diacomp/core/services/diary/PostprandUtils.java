package org.bosik.diacomp.core.services.diary;

import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.utils.Utils;

public class PostprandUtils
{
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
}
