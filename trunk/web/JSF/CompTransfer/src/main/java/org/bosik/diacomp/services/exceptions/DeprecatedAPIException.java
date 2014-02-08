package org.bosik.diacomp.services.exceptions;

public class DeprecatedAPIException extends AuthException
{
	private static final long	serialVersionUID	= 1L;

	public DeprecatedAPIException(Exception e)
	{
		super(e);
	}

	public DeprecatedAPIException(int expectedVersion, int actualVersion)
	{
		super(String.format("API %d is deprecated, %d or later expected", actualVersion, expectedVersion));
	}
}
