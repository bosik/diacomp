package org.bosik.compensation.persistence.serializers.diary;

import junit.framework.TestCase;
import org.bosik.compensation.bo.diary.DiaryPage;
import org.bosik.compensation.fakes.mocks.DiaryPageUtils;
import org.bosik.compensation.persistence.serializers.Serializer;

public abstract class DiaryPageSerializerTest extends TestCase
{
	protected abstract Serializer<DiaryPage> getSerializer();

	public void testPersistenceSingle()
	{
		Serializer<DiaryPage> serializer = getSerializer();

		DiaryPage org = DiaryPageUtils.demoPageA();
		String source = serializer.write(org);

		DiaryPage restored = serializer.read(source);
		DiaryPageUtils.comparePages(org, restored);
	}
}
