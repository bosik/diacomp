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

import static junit.framework.Assert.fail;
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

	private static void assertArraysEquals(byte[] expected, byte[] actual)
	{
		String complaint = "Expected: " + format(expected) + "\nActual: " + format(actual);

		if (expected == null ^ actual == null)
		{
			fail(complaint);
		}
		else if (expected != null)
		{
			if (expected.length != actual.length)
			{
				fail(complaint);
			}
			else
			{
				for (int i = 0; i < expected.length; i++)
				{
					if (expected[i] != actual[i])
					{
						fail(complaint);
						break;
					}
				}
			}
		}
	}

	private static String format(byte[] b)
	{
		if (b == null)
		{
			return null;
		}

		StringBuilder s = new StringBuilder();
		s.append('[');
		for (int i = 0; i < b.length; i++)
		{
			s.append(b[i]);
			if (i < b.length - 1)
			{
				s.append(", ");
			}
		}
		s.append(']');

		return s.toString();
	}

	@Test
	public void test_strToByte()
	{
		assertArraysEquals(null, HashUtils.strToByte(null));
		assertArraysEquals(new byte[] {}, HashUtils.strToByte(""));
		assertArraysEquals(new byte[] { 0 - 128 }, HashUtils.strToByte("00"));
		assertArraysEquals(new byte[] { 16 - 128 }, HashUtils.strToByte("10"));
		assertArraysEquals(new byte[] { 255 - 128 }, HashUtils.strToByte("ff"));
		assertArraysEquals(new byte[] { 171 - 128, 205 - 128 }, HashUtils.strToByte("abcd"));
	}

	@Test
	public void test_byteToStr()
	{
		assertEquals(null, HashUtils.byteToStr(null));
		assertEquals("", HashUtils.byteToStr(new byte[] {}));
		assertEquals("00", HashUtils.byteToStr(new byte[] { 0 - 128 }));
		assertEquals("10", HashUtils.byteToStr(new byte[] { 16 - 128 }));
		assertEquals("ff", HashUtils.byteToStr(new byte[] { 255 - 128 }));
		assertEquals("abcd", HashUtils.byteToStr(new byte[] { 171 - 128, 205 - 128 }));
	}

	@Test
	public void test_strToByteToStr()
	{
		String s = HashUtils.generateGuid();
		assertEquals(s, HashUtils.byteToStr(HashUtils.strToByte(s)));
	}

	@Test
	@Ignore("This test case is for manual performance check only")
	public void test_performance_byteToStr()
	{
		final List<byte[]> bytes = new ArrayList<>();
		for (int i = 0; i < 1000000; i++)
		{
			bytes.add(HashUtils.strToByte(HashUtils.generateGuid()));
		}

		System.out.printf(Locale.US, "HashUtils.byteToStr(): %.3f ms%n", Profiler.measureInMsec(new Runnable()
		{
			@Override
			public void run()
			{
				for (byte[] b : bytes)
				{
					HashUtils.byteToStr(b);
				}
			}
		}, 100));
	}

	@Test
	@Ignore("This test case is for manual performance check only")
	public void test_performance_toInt()
	{
		final List<String> values = new ArrayList<>();

		for (int i = 0; i < 65536; i++)
		{
			values.add(Integer.toHexString(i).toLowerCase());
		}

		System.out.printf(Locale.US, "HashUtils.toInt(): %.3f ms%n", Profiler.measureInMsec(new Runnable()
		{
			@Override
			public void run()
			{
				for (String value : values)
				{
					HashUtils.toInt(value);
				}
			}
		}, 10000));
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
	public void test_performance_buildHashTree()
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
