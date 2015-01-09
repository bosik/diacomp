package org.bosik.diacomp.web.backend.features.diary.function;

import java.util.Date;
import java.util.List;
import java.util.Map;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.diary.DiaryService;
import org.bosik.diacomp.core.test.fakes.services.FakeDiaryService;

/**
 * NOTE: ignores userId
 * 
 * @author Bosik
 * 
 */
public class FakeDiaryDAO implements DiaryDAO
{
	private DiaryService	diaryService	= new FakeDiaryService(true);

	@Override
	public void delete(int userId, String id)
	{
		diaryService.delete(id);
	}

	@Override
	public Versioned<DiaryRecord> findById(int userId, String guid)
	{
		return diaryService.findById(guid);
	}

	@Override
	public List<Versioned<DiaryRecord>> findByIdPrefix(int userId, String prefix)
	{
		return diaryService.findByIdPrefix(prefix);
	}

	@Override
	public List<Versioned<DiaryRecord>> findChanged(int userId, Date since)
	{
		return diaryService.findChanged(since);
	}

	@Override
	public List<Versioned<DiaryRecord>> findPeriod(int userId, Date startTime, Date endTime, boolean includeRemoved)
	{
		return diaryService.findPeriod(startTime, endTime, includeRemoved);
	}

	@Override
	public void post(int userId, List<Versioned<DiaryRecord>> records)
	{
		diaryService.save(records);
	}

	@Override
	public String getHash(int userId, String prefix)
	{
		return diaryService.getHash(prefix);
	}

	@Override
	public Map<String, String> getHashChildren(int userId, String prefix)
	{
		return diaryService.getHashChildren(prefix);
	}
}
