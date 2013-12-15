package org.bosik.compensation.persistence.serializers;

import java.util.List;
import junit.framework.AssertionFailedError;
import junit.framework.TestCase;
import android.util.Log;

public abstract class SerializerTest<T> extends TestCase
{
	private static final String	TAG	= SerializerTest.class.getSimpleName();

	protected abstract Serializer<T> getSerializer();

	protected abstract List<T> getSamples();

	protected abstract void compare(T exp, T act);

	// ==========================================================================

	private void compareInformative(T exp, T act)
	{
		try
		{
			compare(exp, act);
		}
		catch (AssertionFailedError e)
		{
			Log.e(TAG, "Comparison error:");
			Log.e(TAG, exp.toString());
			Log.e(TAG, act.toString());
			throw e;
		}
	}

	public void testPersistenceSingle()
	{
		Serializer<T> serializer = getSerializer();
		List<T> samples = getSamples();
		assertTrue(samples.size() > 0);

		for (T sample : samples)
		{
			compareInformative(sample, serializer.read(serializer.write(sample)));
		}
	}

	public void testPersistenceMultiple()
	{
		Serializer<T> serializer = getSerializer();
		List<T> samples = getSamples();
		assertTrue(samples.size() > 1);

		List<T> restored = serializer.readAll(serializer.writeAll(samples));
		assertEquals(samples.size(), restored.size());
		for (int i = 0; i < samples.size(); i++)
		{
			compareInformative(samples.get(i), restored.get(i));
		}
	}
}