package org.bosik.compensation.persistence.dao.web.utils.client.exceptions;

/**
 * States one of mandatory fields in empty
 * 
 * @author Bosik
 */
public class UndefinedFieldException extends IllegalArgumentException
{
	private static final long	serialVersionUID	= 3716509470386883692L;
	public boolean				undefServer			= false;
	public boolean				undefLogin			= false;
	public boolean				undefPassword		= false;

	public UndefinedFieldException(boolean undefServer, boolean undefLogin, boolean undefPassword)
	{
		super("These fields are undefined (empty or null): " + (undefServer ? " server;" : "")
				+ (undefLogin ? " username;" : "") + (undefPassword ? " password;" : ""));

		this.undefServer = undefServer;
		this.undefLogin = undefLogin;
		this.undefPassword = undefPassword;
	}
	// public UndefinedFieldException(Throwable throwable) { super(throwable); }
	// public UndefinedFieldException(String detailMessage, Throwable throwable) {
	// super(detailMessage, throwable); }
}
