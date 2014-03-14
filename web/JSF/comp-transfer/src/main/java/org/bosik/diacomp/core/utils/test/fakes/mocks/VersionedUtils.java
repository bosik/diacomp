package org.bosik.diacomp.core.utils.test.fakes.mocks;

import java.util.List;
import org.bosik.diacomp.core.entities.tech.Versioned;

public class VersionedUtils
{
	public static <T> void enumerateGuids(List<Versioned<T>> items)
	{
		int n = 1;
		for (Versioned<T> item : items)
		{
			item.setId(String.valueOf(n++));
		}
	}
}
