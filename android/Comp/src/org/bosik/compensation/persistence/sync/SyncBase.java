package org.bosik.compensation.persistence.sync;

import org.bosik.compensation.persistence.repository.common.Interchangeable;
import android.util.Log;

public class SyncBase
{
	private static final String	TAG	= SyncBase.class.getSimpleName();

	public static enum SyncResult
	{
		FIRST_UPDATED, SECOND_UPDATED, EQUAL
	}

	public static SyncResult synchronize(Interchangeable source1, Interchangeable source2)
	{
		int version1 = source1.getVersion();
		int version2 = source2.getVersion();

		Log.i(TAG, "version1 = " + version1);
		Log.i(TAG, "version2 = " + version2);

		if (version1 > version2)
		{
			source2.postData(source1.getData());
			return SyncResult.SECOND_UPDATED;
		}

		if (version1 < version2)
		{
			source1.postData(source2.getData());
			return SyncResult.FIRST_UPDATED;
		}

		return SyncResult.EQUAL;
	}
}
