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
package org.bosik.diacomp.android.backend.features.sync;

import org.bosik.diacomp.android.frontend.activities.ActivityLogin;
import android.accounts.AbstractAccountAuthenticator;
import android.accounts.Account;
import android.accounts.AccountAuthenticatorResponse;
import android.accounts.AccountManager;
import android.accounts.NetworkErrorException;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;

public class Authenticator extends AbstractAccountAuthenticator
{
	private final Context	mContext;

	public Authenticator(Context context)
	{
		super(context);
		mContext = context;
	}

	@Override
	public Bundle editProperties(AccountAuthenticatorResponse r, String s)
	{
		throw new UnsupportedOperationException();
	}

	@Override
	public Bundle addAccount(AccountAuthenticatorResponse response, String accountType, String authTokenType,
			String[] requiredFeatures, Bundle options) throws NetworkErrorException
	{
		final Intent intent = new Intent(mContext, ActivityLogin.class);
		intent.putExtra(ActivityLogin.ARG_ACCOUNT_TYPE, accountType);
		intent.putExtra(ActivityLogin.ARG_AUTH_TYPE, authTokenType);
		intent.putExtra(ActivityLogin.ARG_IS_ADDING_NEW_ACCOUNT, true);
		intent.putExtra(AccountManager.KEY_ACCOUNT_AUTHENTICATOR_RESPONSE, response);
		final Bundle bundle = new Bundle();
		bundle.putParcelable(AccountManager.KEY_INTENT, intent);
		return bundle;
	}

	// Ignore attempts to confirm credentials
	@Override
	public Bundle confirmCredentials(AccountAuthenticatorResponse r, Account account, Bundle bundle)
			throws NetworkErrorException
	{
		return null;
	}

	// Getting an authentication token is not supported
	@Override
	public Bundle getAuthToken(AccountAuthenticatorResponse response, Account account, String authTokenType,
			Bundle options) throws NetworkErrorException
	{
		// Extract the username and password from the Account Manager, and ask
		// the server for an appropriate AuthToken.
		final AccountManager am = AccountManager.get(mContext);

		String authToken = am.peekAuthToken(account, authTokenType);

		// Lets give another try to authenticate the user
		if (TextUtils.isEmpty(authToken))
		{
			final String password = am.getPassword(account);
			if (password != null)
			{
				authToken = "";// sServerAuthenticate.userSignIn(account.name, password,
								// authTokenType);
			}
		}

		// If we get an authToken - we return it
		if (!TextUtils.isEmpty(authToken))
		{
			final Bundle result = new Bundle();
			result.putString(AccountManager.KEY_ACCOUNT_NAME, account.name);
			result.putString(AccountManager.KEY_ACCOUNT_TYPE, account.type);
			result.putString(AccountManager.KEY_AUTHTOKEN, authToken);
			return result;
		}

		// If we get here, then we couldn't access the user's password - so we
		// need to re-prompt them for their credentials. We do that by creating
		// an intent to display our AuthenticatorActivity.
		final Intent intent = new Intent(mContext, ActivityLogin.class);
		intent.putExtra(AccountManager.KEY_ACCOUNT_AUTHENTICATOR_RESPONSE, response);
		intent.putExtra(ActivityLogin.ARG_ACCOUNT_TYPE, account.type);
		intent.putExtra(ActivityLogin.ARG_AUTH_TYPE, authTokenType);
		final Bundle bundle = new Bundle();
		bundle.putParcelable(AccountManager.KEY_INTENT, intent);
		return bundle;
	}

	// Getting a label for the auth token is not supported
	@Override
	public String getAuthTokenLabel(String s)
	{
		throw new UnsupportedOperationException();
	}

	// Updating user credentials is not supported
	@Override
	public Bundle updateCredentials(AccountAuthenticatorResponse r, Account account, String s, Bundle bundle)
			throws NetworkErrorException
	{
		throw new UnsupportedOperationException();
	}

	// Checking features for the account is not supported
	@Override
	public Bundle hasFeatures(AccountAuthenticatorResponse r, Account account, String[] strings)
			throws NetworkErrorException
	{
		throw new UnsupportedOperationException();
	}
}
