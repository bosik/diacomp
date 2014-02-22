package org.bosik.diacomp.core.services.exceptions;

public class DuplicateException extends PersistenceException
{
	private static final long	serialVersionUID	= 1L;

	public DuplicateException(String id)
	{
		super(String.format("Item '%s' already exists", id));
	}
}