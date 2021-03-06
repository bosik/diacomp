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
package org.bosik.diacomp.android.backend.features.analyze;

import android.content.Context;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.services.analyze.AnalyzeCore;
import org.bosik.diacomp.core.services.analyze.RateService;
import org.bosik.diacomp.core.services.analyze.entities.Rate;
import org.bosik.diacomp.core.services.analyze.entities.RateList;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

import java.util.Date;
import java.util.List;

public class RateServiceAuto implements RateService
{
	private final DiaryService diaryService;
	private final AnalyzeCore  analyzeCore;
	private final RatesDao     ratesDao;
	private final int          analyzePeriod;
	private final double       adaptation;

	/**
	 * @param diaryService
	 * @param analyzeCore
	 * @param analyzePeriod In days
	 * @param adaptation    [0 .. 0.1]
	 */
	public RateServiceAuto(Context context, DiaryService diaryService, AnalyzeCore analyzeCore, int analyzePeriod, double adaptation)
	{
		this.diaryService = diaryService;
		this.analyzeCore = analyzeCore;
		this.ratesDao = new RatesDao(context);
		this.analyzePeriod = analyzePeriod;
		this.adaptation = adaptation;
	}

	@Override
	public void update()
	{
		Date timeTo = new Date();
		Date timeFrom = new Date(timeTo.getTime() - (analyzePeriod * Utils.MsecPerDay));
		List<Versioned<DiaryRecord>> recs = diaryService.findPeriod(timeFrom, timeTo, false);
		RateList rateList = analyzeCore.analyze(recs);

		// So if rates are not calculated properly, the latest successful version will be used
		// instead -- great
		if (validate(rateList))
		{
			ratesDao.save(rateList);
		}
	}

	private static boolean validate(RateList rateList)
	{
		if (rateList == null)
		{
			return false;
		}

		for (int i = 0; i < Utils.MinPerDay; i++)
		{
			Rate rate = rateList.getRate(i);
			if (Double.isNaN(rate.getK()) || Double.isNaN(rate.getQ()) || Double.isNaN(rate.getP()))
			{
				return false;
			}
		}

		return true;
	}

	@Override
	public Rate getRate(int time)
	{
		Rate rate = ratesDao.find(time % Utils.MinPerDay);
		return rate != null ? rate : Utils.STD_COEFFICIENT;
	}
}
