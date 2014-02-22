package org.bosik.diacomp.fakes.mocks;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Random;
import junit.framework.Assert;
import org.bosik.diacomp.core.fakes.mocks.Mock;
import org.bosik.diacomp.core.persistence.common.Versioned;
import org.bosik.diacomp.core.utils.TestUtils;

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
		Assert.assertNotNull(exp);
		Assert.assertNotNull(act);

		// try
		// {
		Assert.assertEquals(exp.getId(), act.getId());
		Assert.assertEquals(exp.getTimeStamp().getTime(), act.getTimeStamp().getTime(), TestUtils.EPS_TIME);
		Assert.assertEquals(exp.getVersion(), act.getVersion());
		Assert.assertEquals(exp.isDeleted(), act.isDeleted());
		Assert.assertEquals(exp, act);
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
