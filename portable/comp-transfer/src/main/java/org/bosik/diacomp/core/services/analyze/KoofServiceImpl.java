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
package org.bosik.diacomp.core.services.analyze;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.services.analyze.entities.Koof;
import org.bosik.diacomp.core.services.analyze.entities.KoofList;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

public class KoofServiceImpl implements KoofService
{
	private final DiaryService	diaryService;
	private final AnalyzeCore	analyzeCore;
	private final int			analyzePeriod;
	private final double		adaptation;

	private KoofList			koofs;

	private static final Koof	STD_KOOF	= new Koof();
	{
		STD_KOOF.setK(0.25);
		STD_KOOF.setQ(2.5);
		STD_KOOF.setP(0.0);
	}

	/**
	 * 
	 * @param diaryService
	 * @param analyzeCore
	 * @param analyzePeriod
	 *            In days
	 * @param adaptation
	 *            [0 .. 0.1]
	 */
	public KoofServiceImpl(DiaryService diaryService, AnalyzeCore analyzeCore, int analyzePeriod, double adaptation)
	{
		this.diaryService = diaryService;
		this.analyzeCore = analyzeCore;
		this.analyzePeriod = analyzePeriod;
		this.adaptation = adaptation;
	}

	@Override
	public void update()
	{
		Date timeTo = new Date();
		Date timeFrom = new Date(timeTo.getTime() - (analyzePeriod * Utils.MsecPerDay));
		List<Versioned<DiaryRecord>> recs = diaryService.findPeriod(timeFrom, timeTo, false);
		koofs = analyzeCore.analyze(recs);
	}

	@Override
	public Koof getKoof(int time)
	{
		if (koofs == null)
		{
			update();
		}

		// that means analyzing failed (f.e. if no diary records found)
		if (koofs == null)
		{
			return STD_KOOF;
		}

		return koofs.getKoof(time);
	}
}
