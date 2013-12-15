package org.bosik.compensation.persistence.serializers;

import java.util.List;
import junit.framework.TestCase;

public abstract class SerializerTest<T> extends TestCase
{
	protected abstract Serializer<T> getSerializer();

	protected abstract List<T> getSamples();

	protected abstract void compare(T a, T b);

	// ==========================================================================

	public void testPersistenceSingle()
	{
		Serializer<T> serializer = getSerializer();
		List<T> samples = getSamples();
		assertTrue(samples.size() > 0);

		for (T sample : samples)
		{
			compare(sample, serializer.read(serializer.write(sample)));
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
			compare(samples.get(i), restored.get(i));
		}
	}
}