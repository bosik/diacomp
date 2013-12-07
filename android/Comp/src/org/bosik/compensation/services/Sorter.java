package org.bosik.compensation.services;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import org.bosik.compensation.bo.RelativeTagged;

public class Sorter
{
	public enum Sort
	{
		ALPHABET, RELEVANT
	}

	private static final Comparator<RelativeTagged>	COMPARATOR_ALPHABET	= new Comparator<RelativeTagged>()
																		{
																			@Override
																			public int compare(RelativeTagged lhs,
																					RelativeTagged rhs)
																			{
																				return lhs.getName().compareTo(
																						rhs.getName());
																			}
																		};
	private static final Comparator<RelativeTagged>	COMPARATOR_RELEVANT	= new Comparator<RelativeTagged>()
																		{
																			@Override
																			public int compare(RelativeTagged lhs,
																					RelativeTagged rhs)
																			{
																				if (lhs.getTag() == rhs.getTag())
																				{
																					return COMPARATOR_ALPHABET.compare(
																							lhs, rhs);
																				}
																				else
																				{
																					return rhs.getTag() - lhs.getTag();
																				}
																			}
																		};

	public static <T extends RelativeTagged> void sort(List<T> list, Sort order)
	{
		switch (order)
		{
			case ALPHABET:
				Collections.sort(list, COMPARATOR_ALPHABET);
				break;
			case RELEVANT:
				Collections.sort(list, COMPARATOR_RELEVANT);
				break;
		}
	}
}
