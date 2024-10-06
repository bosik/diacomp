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

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Stream;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

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
		assertArraysEquals(new byte[] { 0 }, HashUtils.strToByte("00"));
		assertArraysEquals(new byte[] { 16 }, HashUtils.strToByte("10"));
		assertArraysEquals(new byte[] { 255 - 256 }, HashUtils.strToByte("ff"));
		assertArraysEquals(new byte[] { 171 - 256, 205 - 256 }, HashUtils.strToByte("abcd"));
	}

	@Test
	public void test_byteToStr()
	{
		assertEquals(null, HashUtils.byteToStr(null));
		assertEquals("", HashUtils.byteToStr(new byte[] {}));
		assertEquals("00", HashUtils.byteToStr(new byte[] { 0 }));
		assertEquals("10", HashUtils.byteToStr(new byte[] { 16 }));
		assertEquals("ff", HashUtils.byteToStr(new byte[] { 255 - 256 }));
		assertEquals("abcd", HashUtils.byteToStr(new byte[] { 171 - 256, 205 - 256 }));
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
		for (int i = 0; i < 1_000_000; i++)
		{
			bytes.add(HashUtils.strToByte(HashUtils.generateGuid()));
		}

		System.out.printf(Locale.US, "HashUtils.byteToStr(): %.3f ms%n", Profiler.measureInMsec(() -> {
			for (byte[] b : bytes)
			{
				HashUtils.byteToStr(b);
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

		System.out.printf(Locale.US, "HashUtils.toInt(): %.3f ms%n", Profiler.measureInMsec(() -> {
			for (String value : values)
			{
				HashUtils.toInt(value);
			}
		}, 10000));
	}

	@Test
	public void test_sumStrings()
	{
		assertEquals("00000000000000000000000000000000",
				HashUtils.sum("00000000000000000000000000000000", "00000000000000000000000000000000"));
		assertEquals("22222222222222222222222222222222",
				HashUtils.sum("11111111111111111111111111111111", "11111111111111111111111111111111"));
		assertEquals("ffffffffffffffffffffffffffffffff",
				HashUtils.sum("88888888888888888888888888888888", "77777777777777777777777777777777"));
		assertEquals("00000000000000000000000000000000",
				HashUtils.sum("88888888888888888888888888888888", "88888888888888888888888888888888"));
	}

	@Test
	public void test_subHash()
	{
		// corner cases
		assertArraysEquals(new byte[] {}, HashUtils.sub(new byte[] {}, new byte[] {}));
		assertArraysEquals(new byte[] { 1, 2, 3 }, HashUtils.sub(new byte[] { 1, 2, 3 }, null));

		// "000000" - "010203" = "0f0e0d"
		assertArraysEquals(new byte[] { 15, 14, 13 }, HashUtils.sub(new byte[] { 0, 0, 0 }, new byte[] { 1, 2, 3 }));
		assertArraysEquals(new byte[] { 15, 14, 13 }, HashUtils.sub(null, new byte[] { 1, 2, 3 }));

		// normal cases
		assertArraysEquals(new byte[] { 0, 0, 0 }, HashUtils.sub(new byte[] { 1, 2, 3 }, new byte[] { 1, 2, 3 }));
		// "7f" - "ff" = "80"
		assertArraysEquals(new byte[] { -128 }, HashUtils.sub(new byte[] { 127 }, new byte[] { -1 }));

		// "7f" = 127
		// "02" = 2
		// "01" = 1
		// "00" = 0
		// "ff" = -1
		// "fe" = -2
		// ..
		// "82" = -126
		// "81" = -127
		// "80" = -128

		assertArraysEquals(HashUtils.strToByte("8f"), HashUtils.sub(
				HashUtils.strToByte("80"),
				HashUtils.strToByte("01")
		));
	}

	@Test
	public void test_addSubHash()
	{
		// given
		final byte[] a = { 127, -128, 0 };
		final byte[] b = { 4, 5, 6 };

		// when / then
		assertArraysEquals(new byte[] { 127, -128, 0 }, HashUtils.sub(HashUtils.add(a, b), b));
	}

	@Test
	public void test_addHash()
	{
		// given
		final byte[] data = new byte[] { 0, 0, 0, 0 };

		// when
		HashUtils.add(data, null);
		HashUtils.add(data, new byte[] { 0, 0, 0, 0 });
		HashUtils.add(data, new byte[] { 1, 2, 3, 4 });

		// then
		assertEquals(
				HashUtils.byteToStr(new byte[] { 1, 2, 3, 4 }),
				HashUtils.byteToStr(data)
		);
	}

	@Test
	public void test_sumHashes()
	{
		// given
		final List<String> hashes = Arrays.asList(
				"00000000000000000000000000000000",
				"11111111111111111111111111111111",
				"22222222222222222222222222222222"
		);

		// when
		final String sum = HashUtils.sum(hashes);

		// then
		assertEquals("33333333333333333333333333333333", sum);
	}

	@Test
	@Ignore("This test case is for manual performance check only")
	public void hashesLoadingTest()
	{
		final double time = Profiler.measureInMsec(() ->
		{
			try
			{
				loadMap("merkletree/input_hashes.csv");
			}
			catch (Exception e)
			{
				throw new RuntimeException(e);
			}
		}, 100);

		System.out.printf(Locale.US, "loadHashes(): %.3f ms%n", time);
		// unsorted: 73.64 ms
		// sorted: 41.668 ms (44% faster than unsorted)
	}

	private static Map<String, String> loadMap(String resourceName) throws IOException, URISyntaxException
	{
		final URI uri = TestHashUtils.class.getClassLoader()
				.getResource(resourceName)
				.toURI();
		return loadMap(new File(uri).toPath());
	}

	private static Map<String, String> loadMap(Path fileName) throws IOException
	{
		final Map<String, String> data = new HashMap<>();

		try (Stream<String> lines = Files.lines(fileName))
		{
			lines.skip(1) // header
					.forEach(line ->
					{
						final int k = line.indexOf('\t');
						final String id = line.substring(0, k);
						final String hash = line.substring(k + 1);
						data.put(id, hash);
					});
		}

		return data;
	}
}
