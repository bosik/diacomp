package org.bosik.diacomp.persistence.services.web.utils.client.exceptions;

/**
 * Internet connection error
 * 
 * @author Bosik
 */
public class ConnectionException extends WebClientException
{
	private static final long	serialVersionUID	= 5396386468370646791L;

	public ConnectionException(String detailMessage)
	{
		super(detailMessage);
	}

	public ConnectionException(Throwable throwable)
	{
		super(throwable);
	}

	public ConnectionException(String detailMessage, Throwable throwable)
	{
		super(detailMessage, throwable);
	}
}