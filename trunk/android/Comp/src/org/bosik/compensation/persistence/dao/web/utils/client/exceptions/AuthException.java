package org.bosik.compensation.persistence.dao.web.utils.client.exceptions;

/**
 * Authentication exception
 * 
 * @author Bosik
 */
public class AuthException extends WebClientException
{
	private static final long	serialVersionUID	= 7885618396446513997L;

	public AuthException(String detailMessage)
	{
		super(detailMessage);
	}
}
