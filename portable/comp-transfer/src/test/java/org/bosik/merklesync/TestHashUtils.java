/*
 * MerkleSync - Data synchronization routine based on Merkle hash trees
 * Copyright (C) 2013 Nikita Bosik
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.bosik.merklesync;

import org.bosik.diacomp.core.utils.Profiler;
import org.junit.Ignore;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.SortedMap;
import java.util.TreeMap;

import static junit.framework.TestCase.assertEquals;

@SuppressWarnings("static-method")
public class TestHashUtils
{
	@Test
	public void test_toInt()
	{
		assertEquals(0, HashUtils.toInt(""));
		assertEquals(0, HashUtils.toInt("0"));
		assertEquals(1, HashUtils.toInt("1"));
		assertEquals(15, HashUtils.toInt("f"));
		assertEquals(16, HashUtils.toInt("10"));
		assertEquals(17, HashUtils.toInt("11"));
		assertEquals(250, HashUtils.toInt("fa"));
		assertEquals(4085, HashUtils.toInt("ff5"));
		assertEquals(65535, HashUtils.toInt("ffff"));
	}

	@Test
	@Ignore
	public void test_toInt_speed()
	{
		final List<String> values = new ArrayList<>();

		for (int i = 0; i < 65536; i++)
		{
			values.add(Integer.toHexString(i).toLowerCase());
		}

		double time = Profiler.measureInMsec(new Runnable()
		{
			@Override
			public void run()
			{
				for (String value : values)
				{
					HashUtils.toInt(value);
				}
			}
		}, 10000);

		System.out.printf(Locale.US, "HashUtils.toInt(): %.3f ms%n", time);
	}

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
	@Ignore("This test case is for manual performance check only")
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
