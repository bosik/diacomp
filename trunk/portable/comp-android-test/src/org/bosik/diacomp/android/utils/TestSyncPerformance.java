package org.bosik.diacomp.android.utils;

import java.util.SortedMap;
import java.util.TreeMap;
import junit.framework.TestCase;
import org.bosik.diacomp.core.services.sync.HashUtils;
import org.bosik.diacomp.core.utils.Utils;
import android.util.Log;

@SuppressWarnings("static-method")
public class TestSyncPerformance extends TestCase
{
	private static final String	TAG	= TestSyncPerformance.class.getSimpleName();

	public void testSyncPerformance()
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
		Log.i(TAG, String.format("%d items prepared in %d ms", data.size(), time));

		time = System.currentTimeMillis();
		SortedMap<String, String> tree = HashUtils.buildHashTree(data);
		time = System.currentTimeMillis() - time;

		Log.i(TAG, String.format("Tree with %d items build in %d ms", tree.size(), time));
	}
}
