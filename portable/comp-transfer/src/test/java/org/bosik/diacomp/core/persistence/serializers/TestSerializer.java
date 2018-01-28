/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.core.persistence.serializers;

import junit.framework.AssertionFailedError;
import org.bosik.diacomp.core.mocks.Mock;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;

public abstract class TestSerializer<T>
{
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
			System.err.println("Comparison error:");
			System.err.println("\t" + exp.toString());
			System.err.println("\t" + act.toString());
			throw e;
		}
	}

	@Test
	public void testPersistenceSingle()
	{
		Serializer<T> serializer = getSerializer();
		List<T> samples = getMock().getSamples();
		Assert.assertTrue(samples.size() > 0);

		for (T sample : samples)
		{
			compareInformative(sample, serializer.read(serializer.write(sample)));
		}
	}

	@Test
	public void testPersistenceMultiple()
	{
		Serializer<T> serializer = getSerializer();
		List<T> samples = getMock().getSamples();
		Assert.assertTrue(samples.size() > 1);

		List<T> restored = serializer.readAll(serializer.writeAll(samples));
		Assert.assertEquals(samples.size(), restored.size());
		for (int i = 0; i < samples.size(); i++)
		{
			compareInformative(samples.get(i), restored.get(i));
		}
	}
}
