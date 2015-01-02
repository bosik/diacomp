package org.bosik.diacomp.core.services.search;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import org.bosik.diacomp.core.entities.business.interfaces.NamedRelativeTagged;
import org.bosik.diacomp.core.entities.tech.Versioned;

public class Sorter<T extends NamedRelativeTagged>
{
	public enum Sort {
		ALPHABET, RELEVANT
	}

	final Comparator<T>				COMPARATOR_ALPHABET				= new Comparator<T>()
																	{
																		@Override
																		public int compare(T lhs, T rhs)
																		{
																			return lhs.getName().compareTo(
																					rhs.getName());
																		}
																	};
	final Comparator<T>				COMPARATOR_RELEVANT				= new Comparator<T>()
																	{
																		@Override
																		public int compare(T lhs, T rhs)
																		{
																			if (lhs.getTag() == rhs.getTag())
																			{
																				return COMPARATOR_ALPHABET.compare(lhs,
																						rhs);
																			}
																			else
																			{
																				return rhs.getTag() - lhs.getTag();
																			}
																		}
																	};

	final Comparator<Versioned<T>>	COMPARATOR_VERSIONED_ALPHABET	= new Comparator<Versioned<T>>()
																	{
																		@Override
																		public int compare(Versioned<T> lhs,
																				Versioned<T> rhs)
																		{
																			return COMPARATOR_ALPHABET.compare(
																					lhs.getData(), rhs.getData());
																		}
																	};
	final Comparator<Versioned<T>>	COMPARATOR_VERSIONED_RELEVANT	= new Comparator<Versioned<T>>()
																	{
																		@Override
																		public int compare(Versioned<T> lhs,
																				Versioned<T> rhs)
																		{
																			return COMPARATOR_RELEVANT.compare(
																					lhs.getData(), rhs.getData());
																		}
																	};

	// TODO: rename this methods

	public void sortData(List<T> list, Sort order)
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

	public void sort(List<Versioned<T>> list, Sort order)
	{
		switch (order)
		{
			case ALPHABET:
				Collections.sort(list, COMPARATOR_VERSIONED_ALPHABET);
				break;
			case RELEVANT:
				Collections.sort(list, COMPARATOR_VERSIONED_RELEVANT);
				break;
		}
	}
}
