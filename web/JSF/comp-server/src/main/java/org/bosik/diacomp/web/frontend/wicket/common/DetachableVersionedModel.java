package org.bosik.diacomp.web.frontend.wicket.common;

import org.apache.wicket.model.LoadableDetachableModel;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.ObjectService;

@Deprecated
/**
 * Under construction. Right now raise problems: source is not serializable
 * @author Bosik
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
