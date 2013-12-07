package org.bosik.compensation.persistence.exceptions;

public class ItemNotFoundException extends RuntimeException
{
	private static final long	serialVersionUID	= 1L;

	public ItemNotFoundException(String id)
	{
		super(String.format("Item '%s' not found", id));
	}
}
