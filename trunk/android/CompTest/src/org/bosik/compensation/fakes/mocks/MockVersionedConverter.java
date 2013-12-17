package org.bosik.compensation.fakes.mocks;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Random;
import junit.framework.TestCase;
import org.bosik.compensation.persistence.common.Versioned;

public class MockVersionedConverter<T> extends TestCase implements Mock<Versioned<T>>
{
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
		assertNotNull(exp);
		assertNotNull(act);

		assertEquals(exp, act);
		assertEquals(exp.getId(), act.getId());
		assertEquals(exp.getTimeStamp(), act.getTimeStamp());
		assertEquals(exp.getVersion(), act.getVersion());

		generator.compare(exp.getData(), act.getData());
	}
}
