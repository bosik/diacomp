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
package org.bosik.diacomp.web.frontend.wicket.common;

import org.apache.wicket.model.LoadableDetachableModel;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.merklesync.Versioned;

@Deprecated
/**
 * Under construction. Right now raise problems: source is not serializable
 * 
 * @param <T>
 */
public class DetachableVersionedModel<T> extends LoadableDetachableModel<Versioned<T>>
{
	private static final long		serialVersionUID	= 1L;

	private final String			id;
	private final ObjectService<T>	source;

	public DetachableVersionedModel(String id, ObjectService<T> source)
	{
		if (id == null)
		{
			throw new IllegalArgumentException();
		}
		this.id = id;
		this.source = source;
	}

	@Override
	public int hashCode()
	{
		return id.hashCode();
	}

	/**
	 * Used for dataview with ReuseIfModelsEqualStrategy item reuse strategy
	 * 
	 * @see org.apache.wicket.markup.repeater.ReuseIfModelsEqualStrategy
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj)
	{
		if (obj == this)
		{
			return true;
		}
		else if (obj == null)
		{
			return false;
		}
		else if (obj instanceof DetachableVersionedModel)
		{
			@SuppressWarnings("unchecked")
			DetachableVersionedModel<T> other = (DetachableVersionedModel<T>)obj;
			return other.id == id;
		}
		return false;
	}

	@Override
	protected Versioned<T> load()
	{
		return source.findById(id);
	}
}
