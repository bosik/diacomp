package org.bosik.diacomp.core.persistence.serializers.diary;

import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.ParserDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.TestSerializer;
import org.bosik.diacomp.core.persistence.serializers.utils.SerializerAdapter;
import org.bosik.diacomp.core.utils.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.utils.test.fakes.mocks.MockDiaryRecord;

public class TestSerializerDiaryRecordJSON extends TestSerializer<DiaryRecord>
{
	private static final Mock<DiaryRecord>				mockFoodItem		= new MockDiaryRecord();
	private static final SerializerAdapter<DiaryRecord>	serializerAdapter	= new SerializerAdapter<DiaryRecord>(
																					new ParserDiaryRecord());

	@Override
	protected Mock<DiaryRecord> getMock()
	{
		return mockFoodItem;
	}

	@Override
	protected Serializer<DiaryRecord> getSerializer()
	{
		return serializerAdapter;
	}
}
