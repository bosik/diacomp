package org.bosik.diacomp.android.persistence.services.web.utils.client.exceptions;

/**
 * Basic web client's exceptions class.
 * 
 * @author Bosik
 */
public class WebClientException extends RuntimeException
{
	private static final long	serialVersionUID	= -4422450897857678241L;

	public WebClientException(String detailMessage)
	{
		super(detailMessage);
	}

	public WebClientException(Throwable throwable)
	{
		super(throwable);
	}

	public WebClientException(String detailMessage, Throwable throwable)
	{
		super(detailMessage, throwable);
	}
}
