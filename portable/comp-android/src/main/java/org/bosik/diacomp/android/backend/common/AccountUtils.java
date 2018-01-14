package org.bosik.diacomp.android.backend.common;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.content.Context;

public class AccountUtils
{
	public static Account[] getAccounts(Context context)
	{
		final String ACCOUNT_TYPE = "diacomp.org";
	
		AccountManager am = AccountManager.get(context);
		Account[] accounts = am.getAccountsByType(ACCOUNT_TYPE);
		return accounts;
	}

	public static Account getAccount(Context context)
	{
		Account[] accounts = getAccounts(context);
		return accounts.length > 0 ? accounts[0] : null;
	}
}
