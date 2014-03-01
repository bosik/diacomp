package org.bosik.diacomp.core.testutils.fakes.mocks;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Random;
import junit.framework.ComparisonFailure;
import junit.framework.TestCase;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.utils.Utils;
import org.junit.Ignore;

@Ignore
public class MockVersionedConverter<T> implements Mock<Versioned<T>>
{
	// private static final String TAG = MockVersionedConverter.class.getSimpleName();
	private Mock<T>	generator;

	public MockVersionedConverter(Mock<T> generator)
	{
		this.generator = generator;
	}

	public List<Versioned<T>> getSamples()
	{
		List<T> samples = generator.getSamples();
		List<Versioned<T>> result = new ArrayList<Versioned<T>>();

		for (T sample : samples)
		{
			Random r = new Random();

			Versioned<T> item = new Versioned<T>(sample);
			item.setId(Utils.getGUID());
			item.setTimeStamp(new Date(1261440000 + r.nextInt(100000000)));
			item.setVersion(r.nextInt(1000000));
			item.setDeleted(true/* r.nextBoolean() */);
			result.add(item);
		}

		return result;
	}

	public void compare(Versioned<T> exp, Versioned<T> act)
	{
		TestCase.assertNotNull(exp);
		TestCase.assertNotNull(act);

		try
		{
			TestCase.assertEquals(exp.getId(), act.getId());
			TestCase.assertEquals(Utils.formatTimeUTC(exp.getTimeStamp()), Utils.formatTimeUTC(act.getTimeStamp()));
			TestCase.assertEquals(exp.getVersion(), act.getVersion());
			TestCase.assertEquals(exp.isDeleted(), act.isDeleted());
			TestCase.assertEquals(exp, act);
			generator.compare(exp.getData(), act.getData());
		}
		catch (ComparisonFailure e)
		{
			// Serializer<Versioned<DiaryRecord>> s = new Diary
			// Log.e(TAG, "Exp: " + exp.getData().toString());
			// Log.e(TAG, "Act: " + act.getData().toString());
			System.out.println("Exp: " + exp.getData().toString());
			System.out.println("Act: " + act.getData().toString());
			throw e;
		}
	}
}
