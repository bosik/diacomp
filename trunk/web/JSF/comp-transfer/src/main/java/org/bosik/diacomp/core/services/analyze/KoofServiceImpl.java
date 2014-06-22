package org.bosik.diacomp.core.services.analyze;

import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.analyze.entities.Koof;
import org.bosik.diacomp.core.services.analyze.entities.KoofList;
import org.bosik.diacomp.core.services.diary.DiaryService;

public class KoofServiceImpl implements KoofService
{
	private final DiaryService	diaryService;
	private final AnalyzeCore	analyzeCore;
	private Date				timeFrom;
	private Date				timeTo;

	private KoofList			koofs;

	// TODO: move hardcoded constants outside
	private static final double	adaptation	= 0.99;		// [0..0.1]
	private static final Koof	STD_KOOF	= new Koof();
	{
		STD_KOOF.setK(0.25);
		STD_KOOF.setQ(2.5);
		STD_KOOF.setP(0.0);
	}

	public KoofServiceImpl(DiaryService diaryService, AnalyzeCore analyzeCore)
	{
		this.diaryService = diaryService;
		this.analyzeCore = analyzeCore;
	}

	@Override
	public void update()
	{
		if (timeFrom == null)
		{
			throw new NullPointerException("timeFrom is null; use setTimeRange() method");
		}
		if (timeTo == null)
		{
			throw new NullPointerException("timeTo is null; use setTimeRange() method");
		}

		List<Versioned<DiaryRecord>> recs = diaryService.findBetween(timeFrom, timeTo, false);
		koofs = AnalyzeExtracter.analyze(recs, analyzeCore, adaptation);
	}

	@Override
	public Koof getKoof(int time)
	{
		if (koofs == null)
		{
			update();
		}

		if (koofs == null)
		{
			return STD_KOOF;
		}

		return koofs.getKoof(time);
	}

	@Override
	public void setTimeRange(Date timeFrom, Date timeTo)
	{
		this.timeFrom = timeFrom;
		this.timeTo = timeTo;
		update();
	}
}