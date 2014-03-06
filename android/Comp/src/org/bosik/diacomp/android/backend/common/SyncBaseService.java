package org.bosik.diacomp.android.backend.common;

import org.bosik.diacomp.core.services.BaseService;

public class SyncBaseService
{
	private static final String	TAG	= SyncBaseService.class.getSimpleName();

	public static enum SyncResult
	{
		FIRST_UPDATED, SECOND_UPDATED, EQUAL
	}

	public static <T> SyncResult synchronize(BaseService<T> source1, BaseService<T> source2)
	{
		throw new RuntimeException("Not implemented yet");

		// int version1 = source1.getVersion();
		// int version2 = source2.getVersion();
		//
		// Log.i(TAG, "version1 = " + version1);
		// Log.i(TAG, "version2 = " + version2);
		//
		// if (version1 > version2)
		// {
		// source2.replaceAll(source1.findAll(), source1.getVersion());
		// return SyncResult.SECOND_UPDATED;
		// }
		//
		// if (version1 < version2)
		// {
		// source1.replaceAll(source2.findAll(), source2.getVersion());
		// return SyncResult.FIRST_UPDATED;
		// }
		//
		// return SyncResult.EQUAL;
	}
}
