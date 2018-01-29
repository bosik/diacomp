/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.core.services.search;

import org.bosik.diacomp.core.entities.business.interfaces.NamedRelativeTagged;
import org.bosik.merklesync.Versioned;

import java.util.Comparator;

public class Sorter
{
	public static Comparator<NamedRelativeTagged> alphabet()
	{
		return new Comparator<NamedRelativeTagged>()
		{
			@Override
			public int compare(NamedRelativeTagged lhs, NamedRelativeTagged rhs)
			{
				return lhs.getName().compareTo(rhs.getName());
			}
		};
	}

	public static Comparator<NamedRelativeTagged> relevance()
	{
		return new Comparator<NamedRelativeTagged>()
		{
			private final Comparator<NamedRelativeTagged> compAlphabet = alphabet();

			@Override
			public int compare(NamedRelativeTagged lhs, NamedRelativeTagged rhs)
			{
				if (lhs.getTag() == rhs.getTag())
				{
					return compAlphabet.compare(lhs, rhs);
				}
				else
				{
					return rhs.getTag() - lhs.getTag();
				}
			}
		};
	}

	public static Comparator<Versioned<? extends NamedRelativeTagged>> versionedAlphabet()
	{
		return new Comparator<Versioned<? extends NamedRelativeTagged>>()
		{
			private final Comparator<NamedRelativeTagged> compAlphabet = alphabet();

			@Override
			public int compare(Versioned<? extends NamedRelativeTagged> lhs, Versioned<? extends NamedRelativeTagged> rhs)
			{
				return compAlphabet.compare(lhs.getData(), rhs.getData());
			}
		};
	}

	public static Comparator<Versioned<? extends NamedRelativeTagged>> versionedRelevance()
	{
		return new Comparator<Versioned<? extends NamedRelativeTagged>>()
		{
			@Override
			public int compare(Versioned<? extends NamedRelativeTagged> lhs, Versioned<? extends NamedRelativeTagged> rhs)
			{
				return relevance().compare(lhs.getData(), rhs.getData());
			}
		};
	}
}
