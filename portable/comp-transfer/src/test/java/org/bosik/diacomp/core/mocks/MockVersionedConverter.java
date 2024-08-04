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
package org.bosik.diacomp.core.mocks;

import junit.framework.ComparisonFailure;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.Versioned;
import org.junit.Ignore;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

@Ignore
public class MockVersionedConverter<T> implements Mock<Versioned<T>>
{
	private Mock<T>	generator;
	private Random	r	= new Random();

	public MockVersionedConverter(Mock<T> generator)
	{
		this.generator = generator;
	}

	@Override
	public List<Versioned<T>> getSamples()
	{
		List<Versioned<T>> result = new ArrayList<>();

		for (int i = 0; i < 10; i++)
		{
			result.add(getSample());
		}

		return result;
	}

	@Override
	public Versioned<T> getSample()
	{
		Versioned<T> item = new Versioned<>(generator.getSample());
		item.setId(HashUtils.generateGuid());
		// long timeBase = 1261440000000L;
		// long timeDelta = ((long)r.nextInt(315360000)) * 1000;
		// item.setTimeStamp(new Date(timeBase + timeDelta));
		item.setTimeStamp(Utils.randomTime());
		item.setHash(HashUtils.generateGuid());
		item.setVersion(r.nextInt(100));
		item.setDeleted(r.nextBoolean());

		return item;
	}

	@Override
	public void compare(Versioned<T> exp, Versioned<T> act)
	{
		assertNotNull(exp);
		assertNotNull(act);

		try
		{
			assertEquals(exp.getId(), act.getId());
			assertEquals(Utils.formatTimeUTC(exp.getTimeStamp()), Utils.formatTimeUTC(act.getTimeStamp()));
			assertEquals(exp.getVersion(), act.getVersion());
			assertEquals(exp.isDeleted(), act.isDeleted());
			assertEquals(exp, act);
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
