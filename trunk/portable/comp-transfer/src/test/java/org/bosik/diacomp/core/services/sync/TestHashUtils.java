package org.bosik.diacomp.core.services.sync;

import java.util.SortedMap;
import java.util.TreeMap;
import junit.framework.TestCase;
import org.bosik.diacomp.core.utils.Utils;

@SuppressWarnings("static-method")
public class TestHashUtils extends TestCase
{
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

	public void test_buildHashTree_performance()
	{
		long time = System.currentTimeMillis();
		SortedMap<String, String> data = new TreeMap<String, String>();
		for (int i = 0; i < 25000; i++)
		{
			String id = Utils.generateGuid();
			String hash = Utils.generateGuid();
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
