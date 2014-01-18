package org.bosik.compensation.fakes.mocks;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Random;
import junit.framework.TestCase;
import org.bosik.compensation.utills.TestUtils;
import org.bosik.diacomp.persistence.common.Versioned;
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
			item.setTimeStamp(new Date(r.nextInt(100000000)));
			item.setVersion(r.nextInt(1000000));
			result.add(item);
		}

		return result;
	}

	public void compare(Versioned<T> exp, Versioned<T> act)
	{
		TestCase.assertNotNull(exp);
		TestCase.assertNotNull(act);

		// try
		// {
		TestCase.assertEquals(exp.getId(), act.getId());
		TestCase.assertEquals(exp.getTimeStamp().getTime(), act.getTimeStamp().getTime(), TestUtils.EPS_TIME);
		TestCase.assertEquals(exp.getVersion(), act.getVersion());
		TestCase.assertEquals(exp.isDeleted(), act.isDeleted());
		TestCase.assertEquals(exp, act);
		generator.compare(exp.getData(), act.getData());
		// }
		// catch (ComparisonFailure e)
		// {
		// Log.e(TAG, "Exp: " + exp.getData().toString());
		// Log.e(TAG, "Act: " + act.getData().toString());
		// throw e;
		// }
	}
}
