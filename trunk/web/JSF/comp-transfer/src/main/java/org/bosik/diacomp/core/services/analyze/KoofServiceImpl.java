package org.bosik.diacomp.core.services.analyze;

import java.util.Date;
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
	private static final double	adaptation	= 0.25;		// [0..0.5]
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

		koofs = AnalyzeExtracter.analyze(analyzeCore, diaryService, timeFrom, timeTo, adaptation);
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