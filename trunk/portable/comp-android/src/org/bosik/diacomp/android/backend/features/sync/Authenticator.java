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

/*
 * Implement AbstractAccountAuthenticator and stub out all
 * of its methods
 */
public class Authenticator extends AbstractAccountAuthenticator
{
	private final Context	mContext;

	// Simple constructor
	public Authenticator(Context context)
	{
		super(context);
		mContext = context;
	}

	// Editing properties is not supported
	@Override
	public Bundle editProperties(AccountAuthenticatorResponse r, String s)
	{
		throw new UnsupportedOperationException();
	}

	// Don't add additional accounts
	@Override
	public Bundle addAccount(AccountAuthenticatorResponse response, String accountType, String authTokenType,
			String[] requiredFeatures, Bundle options) throws NetworkErrorException
	{
		final Intent intent = new Intent(mContext, ActivityLogin.class);
		// intent.putExtra(ActivityLogin.ARG_ACCOUNT_TYPE, accountType);
		// intent.putExtra(ActivityLogin.ARG_AUTH_TYPE, authTokenType);
		// intent.putExtra(ActivityLogin.ARG_IS_ADDING_NEW_ACCOUNT, true);
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
	public Bundle getAuthToken(AccountAuthenticatorResponse r, Account account, String s, Bundle bundle)
			throws NetworkErrorException
	{
		throw new UnsupportedOperationException();
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
