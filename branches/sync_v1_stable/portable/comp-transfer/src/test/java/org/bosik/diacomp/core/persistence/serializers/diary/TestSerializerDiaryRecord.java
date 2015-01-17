package org.bosik.diacomp.core.persistence.serializers.diary;

import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.TestSerializer;
import org.bosik.diacomp.core.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.test.fakes.mocks.MockDiaryRecord;
import org.bosik.diacomp.core.test.fakes.mocks.MockVersionedConverter;

public class TestSerializerDiaryRecord extends TestSerializer<Versioned<DiaryRecord>>
{
	@Override
	protected Mock<Versioned<DiaryRecord>> getMock()
	{
		return new MockVersionedConverter<DiaryRecord>(new MockDiaryRecord());
	}

	@Override
	protected Serializer<Versioned<DiaryRecord>> getSerializer()
	{
		return new SerializerDiaryRecord();
	}
}
