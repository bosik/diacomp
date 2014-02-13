package org.bosik.diacomp.backend.dao;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.bosik.diacomp.bo.diary.DiaryRecord;
import org.bosik.diacomp.fakes.mocks.Mock;
import org.bosik.diacomp.fakes.mocks.MockDiaryRecord;
import org.bosik.diacomp.fakes.mocks.MockVersionedConverter;
import org.bosik.diacomp.persistence.common.Versioned;
import org.bosik.diacomp.persistence.serializers.Parser;
import org.bosik.diacomp.persistence.serializers.ParserDiaryRecord;
import org.bosik.diacomp.persistence.serializers.Serializer;
import org.bosik.diacomp.persistence.serializers.utils.SerializerAdapter;

public class FakeDiaryDAO implements IDiaryDAO
{
	private static Mock<Versioned<DiaryRecord>>	mock		= new MockVersionedConverter<DiaryRecord>(
																	new MockDiaryRecord());
	private static List<Versioned<DiaryRecord>>	samples		= mock.getSamples();
	private static Parser<DiaryRecord>			parser		= new ParserDiaryRecord();
	private static Serializer<DiaryRecord>		serializer	= new SerializerAdapter<DiaryRecord>(parser);

	@Override
	public List<Versioned<String>> findMod(int userId, Date time, boolean includeRemoved)
	{
		List<Versioned<String>> result = new ArrayList<Versioned<String>>();

		for (Versioned<DiaryRecord> rec : samples)
		{
			if (rec.getTimeStamp().after(time) && (includeRemoved || !rec.isDeleted()))
			{
				Versioned<String> item = new Versioned<String>();
				item.setId(rec.getId());
				item.setTimeStamp(rec.getTimeStamp());
				item.setVersion(rec.getVersion());
				item.setDeleted(rec.isDeleted());
				item.setData(serializer.write(rec.getData()));
				result.add(item);
			}
		}

		return result;
	}

	@Override
	public List<Versioned<String>> findPeriod(int userId, Date startTime, Date endTime, boolean includeRemoved)
	{
		List<Versioned<String>> result = new ArrayList<Versioned<String>>();

		for (Versioned<DiaryRecord> rec : samples)
		{
			final DiaryRecord data = rec.getData();
			if (data.getTime().after(startTime) && data.getTime().before(endTime)
					&& (includeRemoved || !rec.isDeleted()))
			{
				Versioned<String> item = new Versioned<String>();
				item.setId(rec.getId());
				item.setTimeStamp(rec.getTimeStamp());
				item.setVersion(rec.getVersion());
				item.setDeleted(rec.isDeleted());
				item.setData(serializer.write(data));
				result.add(item);
			}
		}

		return result;
	}
}
