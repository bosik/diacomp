package org.bosik.diacomp.web.backend.features.diary.function;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Parser;
import org.bosik.diacomp.core.persistence.serializers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.utils.SerializerAdapter;
import org.bosik.diacomp.core.utils.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.utils.test.fakes.mocks.MockDiaryRecord;
import org.bosik.diacomp.core.utils.test.fakes.mocks.MockVersionedConverter;

public class FakeDiaryDAO implements DiaryDAO
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

	@Override
	public void post(int userId, List<Versioned<DiaryRecord>> records)
	{
		// TODO Auto-generated method stub
	}

	@Override
	public List<Versioned<String>> findGuid(int userId, String guid)
	{
		List<Versioned<String>> result = new ArrayList<Versioned<String>>();

		for (Versioned<DiaryRecord> rec : samples)
		{
			final DiaryRecord data = rec.getData();
			if (rec.getId().equals(guid))
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
