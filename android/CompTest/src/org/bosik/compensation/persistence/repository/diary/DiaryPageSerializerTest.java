package org.bosik.compensation.persistence.repository.diary;

import junit.framework.TestCase;
import org.bosik.compensation.persistence.entity.diary.DiaryPage;
import org.bosik.compensation.persistence.repository.common.Serializer;

public class DiaryPageSerializerTest extends TestCase
{
	private final Serializer<DiaryPage>	serializer	= new DiaryPageSerializer();

	public void testPersistence()
	{
		DiaryPage org = DiaryPageUtils.demoPageA();
		String source = serializer.write(org);

		DiaryPage restored = serializer.read(source);
		DiaryPageUtils.comparePages(org, restored);
	}
}
