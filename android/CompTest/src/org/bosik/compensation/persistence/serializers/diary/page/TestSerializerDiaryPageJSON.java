package org.bosik.compensation.persistence.serializers.diary.page;

import org.bosik.compensation.bo.diary.DiaryPage;
import org.bosik.compensation.fakes.mocks.Mock;
import org.bosik.compensation.fakes.mocks.MockDiaryPage;
import org.bosik.compensation.persistence.serializers.ParserDiaryPage;
import org.bosik.compensation.persistence.serializers.Serializer;
import org.bosik.compensation.persistence.serializers.TestSerializer;
import org.bosik.compensation.persistence.serializers.utils.SerializerAdapter;

public class TestSerializerDiaryPageJSON extends TestSerializer<DiaryPage>
{
	private static final Mock<DiaryPage>				mockFoodItem		= new MockDiaryPage();
	private static final SerializerAdapter<DiaryPage>	serializerAdapter	= new SerializerAdapter<DiaryPage>(
																					new ParserDiaryPage());

	@Override
	protected Mock<DiaryPage> getMock()
	{
		return mockFoodItem;
	}

	@Override
	protected Serializer<DiaryPage> getSerializer()
	{
		return serializerAdapter;
	}
}
