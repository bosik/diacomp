/*  
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *  
 */
package org.bosik.diacomp.android.backend.common.webclient.exceptions;

/**
 * States one of mandatory fields in empty
 */
public class UndefinedFieldException extends IllegalArgumentException
{
	private static final long    serialVersionUID = 3716509470386883692L;
	private              boolean undefServer      = false;
	private              boolean undefLogin       = false;
	private              boolean undefPassword    = false;

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
