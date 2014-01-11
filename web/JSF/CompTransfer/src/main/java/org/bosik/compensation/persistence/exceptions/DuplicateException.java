package org.bosik.compensation.persistence.exceptions;

public class DuplicateException extends StoreException
{
	private static final long	serialVersionUID	= 1L;

	public DuplicateException(String id)
	{
		super(String.format("Item '%s' already exists", id));
	}
}