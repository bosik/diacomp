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
package org.bosik.diacomp.core.services.sync;

import static junit.framework.TestCase.assertEquals;
import java.util.SortedMap;
import java.util.TreeMap;
import org.bosik.merklesync.HashUtils;
import org.junit.Ignore;
import org.junit.Test;

@SuppressWarnings("static-method")
public class TestHashUtils
{
	@Test
	public void test_sumHash()
	{
		assertEquals("00000000000000000000000000000000",
				HashUtils.sumHash("00000000000000000000000000000000", "00000000000000000000000000000000"));
		assertEquals("22222222222222222222222222222222",
				HashUtils.sumHash("11111111111111111111111111111111", "11111111111111111111111111111111"));
		assertEquals("ffffffffffffffffffffffffffffffff",
				HashUtils.sumHash("88888888888888888888888888888888", "77777777777777777777777777777777"));
		assertEquals("00000000000000000000000000000000",
				HashUtils.sumHash("88888888888888888888888888888888", "88888888888888888888888888888888"));
	}

	@Test
	public void test_subHash()
	{
		assertEquals("00000000000000000000000000000000",
				HashUtils.subHash("00000000000000000000000000000000", "00000000000000000000000000000000"));
		assertEquals("00000000000000000000000000000000",
				HashUtils.subHash("11111111111111111111111111111111", "11111111111111111111111111111111"));
		assertEquals("11111111111111111111111111111111",
				HashUtils.subHash("88888888888888888888888888888888", "77777777777777777777777777777777"));
		assertEquals("0000000000000000000000000000000f",
				HashUtils.subHash("00000000000000000000000000000000", "00000000000000000000000000000001"));
	}

	@Test
	@Ignore
	// This test case is for manual performance checking only
	public void test_buildHashTree_performance()
	{
		long time = System.currentTimeMillis();
		SortedMap<String, String> data = new TreeMap<String, String>();
		for (int i = 0; i < 500000; i++)
		{
			String id = HashUtils.generateGuid();
			String hash = HashUtils.generateGuid();
			data.put(id, hash);
		}
		time = System.currentTimeMillis() - time;
		System.out.println(String.format("%d items prepared in %d ms", data.size(), time));

		time = System.currentTimeMillis();
		SortedMap<String, String> tree = HashUtils.buildHashTree(data);
		time = System.currentTimeMillis() - time;

		System.out.println(String.format("Tree with %d items build in %d ms", tree.size(), time));
	}
}
