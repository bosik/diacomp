package org.bosik.compensation.services;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import org.bosik.compensation.bo.RelativeTagged;
import org.bosik.compensation.persistence.common.Versioned;

public class Sorter<T extends RelativeTagged>
{
	public enum Sort
	{
		ALPHABET, RELEVANT
	}

	final Comparator<Versioned<T>>	COMPARATOR_ALPHABET	= new Comparator<Versioned<T>>()
														{
															@Override
															public int compare(Versioned<T> lhs, Versioned<T> rhs)
															{
																return lhs.getData().getName()
																		.compareTo(rhs.getData().getName());
															}
														};
	final Comparator<Versioned<T>>	COMPARATOR_RELEVANT	= new Comparator<Versioned<T>>()
														{
															@Override
															public int compare(Versioned<T> lhs, Versioned<T> rhs)
															{
																if (lhs.getData().getTag() == rhs.getData().getTag())
																{
																	return COMPARATOR_ALPHABET.compare(lhs, rhs);
																}
																else
																{
																	return rhs.getData().getTag()
																			- lhs.getData().getTag();
																}
															}
														};

	public void sort(List<Versioned<T>> list, Sort order)
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
