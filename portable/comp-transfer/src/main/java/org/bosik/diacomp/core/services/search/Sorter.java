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

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import org.bosik.diacomp.core.entities.business.interfaces.NamedRelativeTagged;
import org.bosik.merklesync.Versioned;

public class Sorter
{
	public enum Sort {
		ALPHABET, RELEVANT
	}

	final Comparator<NamedRelativeTagged>						COMPARATOR_ALPHABET				= new Comparator<NamedRelativeTagged>()
																								{
																									@Override
																									public int compare(
																											NamedRelativeTagged lhs,
																											NamedRelativeTagged rhs)
																									{
																										return lhs
																												.getName()
																												.compareTo(
																														rhs.getName());
																									}
																								};
	final Comparator<NamedRelativeTagged>						COMPARATOR_RELEVANT				= new Comparator<NamedRelativeTagged>()
																								{
																									@Override
																									public int compare(
																											NamedRelativeTagged lhs,
																											NamedRelativeTagged rhs)
																									{
																										if (lhs.getTag() == rhs
																												.getTag())
																										{
																											return COMPARATOR_ALPHABET
																													.compare(
																															lhs,
																															rhs);
																										}
																										else
																										{
																											return rhs
																													.getTag()
																													- lhs.getTag();
																										}
																									}
																								};

	final Comparator<Versioned<? extends NamedRelativeTagged>>	COMPARATOR_VERSIONED_ALPHABET	= new Comparator<Versioned<? extends NamedRelativeTagged>>()
																								{
																									@Override
																									public int compare(
																											Versioned<? extends NamedRelativeTagged> lhs,
																											Versioned<? extends NamedRelativeTagged> rhs)
																									{
																										return COMPARATOR_ALPHABET
																												.compare(
																														lhs.getData(),
																														rhs.getData());
																									}
																								};
	final Comparator<Versioned<? extends NamedRelativeTagged>>	COMPARATOR_VERSIONED_RELEVANT	= new Comparator<Versioned<? extends NamedRelativeTagged>>()
																								{
																									@Override
																									public int compare(
																											Versioned<? extends NamedRelativeTagged> lhs,
																											Versioned<? extends NamedRelativeTagged> rhs)
																									{
																										return COMPARATOR_RELEVANT
																												.compare(
																														lhs.getData(),
																														rhs.getData());
																									}
																								};

	// TODO: rename this methods

	public void sortData(List<? extends NamedRelativeTagged> list, Sort order)
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

	public void sort(List<Versioned<? extends NamedRelativeTagged>> list, Sort order)
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
