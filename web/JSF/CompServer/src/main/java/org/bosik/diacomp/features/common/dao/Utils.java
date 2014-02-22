package org.bosik.diacomp.features.common.dao;

import java.util.Iterator;

public class Utils
{
	public static int count(Iterator<?> iterator)
	{
		int result = 0;
		while (iterator.hasNext())
		{
			result++;
			iterator.next();
		}
		return result;
	}

	public static StringBuilder commaSeparated(Iterator<String> iterator)
	{
		StringBuilder sb = new StringBuilder();

		while (iterator.hasNext())
		{
			sb.append(iterator.next());
			if (iterator.hasNext())
			{
				sb.append(", ");
			}
		}

		return sb;
	}

	public static StringBuilder separated(Iterator<String> iterator, String separator)
	{
		StringBuilder sb = new StringBuilder();

		while (iterator.hasNext())
		{
			sb.append(iterator.next());
			sb.append(" = ?");
			if (iterator.hasNext())
			{
				sb.append(separator);
			}
		}

		return sb;
	}

	public static StringBuilder commaSeparatedQuests(int count)
	{
		StringBuilder sb = new StringBuilder();

		for (int i = 0; i < count; i++)
		{
			sb.append("?");
			if (i < (count - 1))
			{
				sb.append(", ");
			}
		}

		return sb;
	}
}
