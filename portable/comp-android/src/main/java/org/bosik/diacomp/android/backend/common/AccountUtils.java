package org.bosik.diacomp.android.backend.common;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.content.Context;

public class AccountUtils
{
	public static Account[] getAccounts(Context context)
	{
		final String ACCOUNT_TYPE = "diacomp.org";

		final AccountManager am = AccountManager.get(context);
		return am.getAccountsByType(ACCOUNT_TYPE);
	}

	public static Account getAccount(Context context)
	{
		final Account[] accounts = getAccounts(context);
		return accounts.length > 0 ? accounts[0] : null;
	}

	public static boolean hasAccount(Context context)
	{
		final Account[] accounts = getAccounts(context);
		return accounts.length > 0;
	}
}
