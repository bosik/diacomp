package org.bosik.diacomp.persistence.serializers;

import java.util.List;
import junit.framework.AssertionFailedError;
import junit.framework.TestCase;
import org.bosik.compensation.fakes.mocks.Mock;

public abstract class TestSerializer<T> extends TestCase
{
	// private static final String TAG = TestSerializer.class.getSimpleName();

	// ==========================================================================

	protected abstract Serializer<T> getSerializer();

	protected abstract Mock<T> getMock();

	// ==========================================================================

	private void compareInformative(T exp, T act)
	{
		try
		{
			getMock().compare(exp, act);
		}
		catch (AssertionFailedError e)
		{
			// FIXME
			// Log.e(TAG, "Comparison error:");
			// Log.e(TAG, exp.toString());
			// Log.e(TAG, act.toString());
			throw e;
		}
	}

	public void testPersistenceSingle()
	{
		Serializer<T> serializer = getSerializer();
		List<T> samples = getMock().getSamples();
		assertTrue(samples.size() > 0);

		for (T sample : samples)
		{
			compareInformative(sample, serializer.read(serializer.write(sample)));
		}
	}

	public void testPersistenceMultiple()
	{
		Serializer<T> serializer = getSerializer();
		List<T> samples = getMock().getSamples();
		assertTrue(samples.size() > 1);

		List<T> restored = serializer.readAll(serializer.writeAll(samples));
		assertEquals(samples.size(), restored.size());
		for (int i = 0; i < samples.size(); i++)
		{
			compareInformative(samples.get(i), restored.get(i));
		}
	}
}