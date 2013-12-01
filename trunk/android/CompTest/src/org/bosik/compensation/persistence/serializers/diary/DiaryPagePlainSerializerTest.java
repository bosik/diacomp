package org.bosik.compensation.persistence.serializers.diary;

import junit.framework.TestCase;
import org.bosik.compensation.bo.diary.DiaryPage;
import org.bosik.compensation.persistence.dao.DiaryPageUtils;
import org.bosik.compensation.persistence.serializers.Serializer;
import org.bosik.compensation.persistence.serializers.diary.DiaryPagePlainSerializer;

public class DiaryPagePlainSerializerTest extends TestCase
{
	private final Serializer<DiaryPage>	serializer	= new DiaryPagePlainSerializer();

	public void testPersistence()
	{
		DiaryPage org = DiaryPageUtils.demoPageA();
		String source = serializer.write(org);

		DiaryPage restored = serializer.read(source);
		DiaryPageUtils.comparePages(org, restored);
	}
}
