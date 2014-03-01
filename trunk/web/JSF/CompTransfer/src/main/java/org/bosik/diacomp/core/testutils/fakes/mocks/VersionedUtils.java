package org.bosik.diacomp.core.testutils.fakes.mocks;

import java.util.List;
import org.bosik.diacomp.core.entities.tech.Versioned;

public class VersionedUtils
{
	public static <T> void enumerate(List<Versioned<T>> items)
	{
		int n = 1;
		for (Versioned<T> item : items)
		{
			item.setId(String.valueOf(n++));
		}
	}
}
