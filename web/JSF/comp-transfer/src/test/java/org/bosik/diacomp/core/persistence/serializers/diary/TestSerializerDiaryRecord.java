package org.bosik.diacomp.core.persistence.serializers.diary;

import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.TestSerializer;
import org.bosik.diacomp.core.utils.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.utils.test.fakes.mocks.MockDiaryRecord;
import org.bosik.diacomp.core.utils.test.fakes.mocks.MockVersionedConverter;

public class TestSerializerDiaryRecord extends TestSerializer<Versioned<DiaryRecord>>
{
	private static final Mock<DiaryRecord>					mockFoodItem			= new MockDiaryRecord();
	private static final Mock<Versioned<DiaryRecord>>		mockFoodItemVersioned	= new MockVersionedConverter<DiaryRecord>(
																							mockFoodItem);

	private static final Serializer<Versioned<DiaryRecord>>	serializerAdapter		= new SerializerDiaryRecord();

	@Override
	protected Mock<Versioned<DiaryRecord>> getMock()
	{
		return mockFoodItemVersioned;
	}

	@Override
	protected Serializer<Versioned<DiaryRecord>> getSerializer()
	{
		return serializerAdapter;
	}
}
