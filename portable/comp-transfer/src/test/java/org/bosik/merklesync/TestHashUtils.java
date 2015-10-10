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

import static junit.framework.TestCase.assertEquals;
import java.util.SortedMap;
import java.util.TreeMap;
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
	// This test case is for manual performance check only
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
