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
package org.bosik.diacomp.android.frontend.activities;

import android.accounts.Account;
import android.accounts.AccountAuthenticatorActivity;
import android.accounts.AccountManager;
import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.annotation.TargetApi;
import android.content.BroadcastReceiver;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.text.InputType;
import android.text.TextUtils;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;
import android.widget.TextView;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.common.AccountUtils;
import org.bosik.diacomp.android.backend.common.DiaryContentProvider;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.android.backend.common.webclient.WebClientInternal;
import org.bosik.diacomp.android.backend.features.preferences.account.PreferencesLocalService;
import org.bosik.diacomp.android.backend.features.quickImport.ImportHelper.Progress;
import org.bosik.diacomp.android.backend.features.quickImport.ImportService;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.services.preferences.PreferencesTypedService;
import org.bosik.diacomp.core.utils.Utils;

import java.util.ArrayList;
import java.util.List;

public class ActivityLogin extends AccountAuthenticatorActivity
{
	public static final  String DEFAULT_ACCOUNT_TYPE      = "diacomp.org";
	public static final  String EXTRA_EMAIL               = "org.bosik.diacomp.activityLogin.email";
	public static final  String EXTRA_PASS                = "org.bosik.diacomp.activityLogin.password";
	public static final  String ARG_ACCOUNT_TYPE          = "org.bosik.diacomp.activityLogin.accountType";
	public static final  String ARG_AUTH_TYPE             = "org.bosik.diacomp.activityLogin.authType";
	public static final  String ARG_IS_ADDING_NEW_ACCOUNT = "org.bosik.diacomp.activityLogin.newAccount";
	private static final String KEY_IN_PROGRESS           = "org.bosik.diacomp.activityLogin.inProgress";
	private static final String KEY_STATUS                = "org.bosik.diacomp.activityLogin.status";

	private UserLoginTask mAuthTask = null;

	private String  mAccountType;
	private String  mAuthTokenType;
	private boolean mNewAccount;

	// UI
	private EditText textEmail;
	private EditText textPassword;
	private View     mLoginFormView;
	private View     mLoginStatusView;
	private TextView mLoginStatusMessageView;

	@Override
	protected void onResume()
	{
		super.onResume();
		registerReceiver(receiver, new IntentFilter(ImportService.SERVICE_CALLBACK_ID));
	}

	@Override
	protected void onPause()
	{
		super.onPause();
		unregisterReceiver(receiver);
	}

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		// build UI
		setContentView(R.layout.activity_login);

		// read extras
		mAccountType = getIntent().getStringExtra(ARG_ACCOUNT_TYPE);
		mAuthTokenType = getIntent().getStringExtra(ARG_AUTH_TYPE);
		mNewAccount = getIntent().getBooleanExtra(ARG_IS_ADDING_NEW_ACCOUNT, false);

		textEmail = (EditText) findViewById(R.id.accountName);
		textPassword = (EditText) findViewById(R.id.accountPassword);
		// textPassword.setOnEditorActionListener(new TextView.OnEditorActionListener()
		// {
		// @Override
		// public boolean onEditorAction(TextView textView, int id, KeyEvent keyEvent)
		// {
		// if (id == R.id.login || id == EditorInfo.IME_NULL)
		// {
		// attemptLogin();
		// return true;
		// }
		// return false;
		// }
		// });

		mLoginFormView = findViewById(R.id.login_form);
		mLoginStatusView = findViewById(R.id.login_status);
		mLoginStatusMessageView = (TextView) findViewById(R.id.login_status_message);

		findViewById(R.id.buttonLoginShowPassword).setOnClickListener(new View.OnClickListener()
		{
			@Override
			public void onClick(View view)
			{
				if (textPassword.getInputType() == InputType.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD)
				{
					textPassword.setInputType(InputType.TYPE_TEXT_VARIATION_PASSWORD | InputType.TYPE_CLASS_TEXT);
				}
				else
				{
					textPassword.setInputType(InputType.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD);
				}
			}
		});

		findViewById(R.id.buttonLoginSignIn).setOnClickListener(new View.OnClickListener()
		{
			@Override
			public void onClick(View view)
			{
				attemptLogin();
			}
		});

		findViewById(R.id.buttonLoginRegister).setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				String url = Utils.makeSureEndsWithSlash(getString(R.string.server_url)) + "register/";
				startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse(url)));
			}
		});

		if (savedInstanceState != null)
		{
			if (savedInstanceState.containsKey(KEY_IN_PROGRESS))
			{
				showProgress(savedInstanceState.getBoolean(KEY_IN_PROGRESS));
			}

			if (savedInstanceState.containsKey(KEY_STATUS))
			{
				mLoginStatusMessageView.setText(savedInstanceState.getString(KEY_STATUS));
			}
		}
	}

	@Override
	protected void onSaveInstanceState(Bundle outState)
	{
		super.onSaveInstanceState(outState);

		if (outState != null)
		{
			outState.putBoolean(KEY_IN_PROGRESS, mLoginStatusView.getVisibility() == View.VISIBLE);
			outState.putString(KEY_STATUS, (String) mLoginStatusMessageView.getText());
		}
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu)
	{
		super.onCreateOptionsMenu(menu);
		getMenuInflater().inflate(R.menu.activity_login, menu);
		return true;
	}

	private boolean validate()
	{
		List<View> errorFields = new ArrayList<>();
		textEmail.setError(null);
		textPassword.setError(null);

		String email = textEmail.getText().toString();
		String password = textPassword.getText().toString();

		// Check email address
		if (TextUtils.isEmpty(email))
		{
			textEmail.setError(getString(R.string.login_error_field_required));
			errorFields.add(textEmail);
		}
		else if (!email.contains("@"))
		{
			textEmail.setError(getString(R.string.login_error_invalid_email));
			errorFields.add(textEmail);
		}

		// Check password
		if (TextUtils.isEmpty(password))
		{
			textPassword.setError(getString(R.string.login_error_field_required));
			errorFields.add(textPassword);
		}
		else if (password.length() < 6)
		{
			textPassword.setError(getString(R.string.login_error_invalid_password));
			errorFields.add(textPassword);
		}

		if (errorFields.isEmpty())
		{
			return true;
		}
		else
		{
			errorFields.get(0).requestFocus();
			return false;
		}
	}

	private void attemptLogin()
	{
		if (validate())
		{
			if (mAuthTask != null)
			{
				return;
			}

			mAuthTask = new UserLoginTask();
			mAuthTask.execute(textEmail.getText().toString(), textPassword.getText().toString());
		}
	}

	@TargetApi(Build.VERSION_CODES.HONEYCOMB_MR2)
	private void showProgress(final boolean show)
	{
		if (show)
		{
			View view = getCurrentFocus();
			if (view != null)
			{
				// hide keyboard
				InputMethodManager manager = (InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE);
				manager.hideSoftInputFromWindow(view.getWindowToken(), 0);
			}
		}

		// On Honeycomb MR2 we have the ViewPropertyAnimator APIs, which allow
		// for very easy animations. If available, use these APIs to fade-in
		// the progress spinner.
		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB_MR2)
		{
			int shortAnimTime = getResources().getInteger(android.R.integer.config_shortAnimTime);

			mLoginStatusView.setVisibility(View.VISIBLE);
			mLoginStatusView.animate().setDuration(shortAnimTime).alpha(show ? 1 : 0).setListener(new AnimatorListenerAdapter()
			{
				@Override
				public void onAnimationEnd(Animator animation)
				{
					mLoginStatusView.setVisibility(show ? View.VISIBLE : View.GONE);
				}
			});

			mLoginFormView.setVisibility(View.VISIBLE);
			mLoginFormView.animate().setDuration(shortAnimTime).alpha(show ? 0 : 1).setListener(new AnimatorListenerAdapter()
			{
				@Override
				public void onAnimationEnd(Animator animation)
				{
					mLoginFormView.setVisibility(show ? View.GONE : View.VISIBLE);
				}
			});
		}
		else
		{
			// The ViewPropertyAnimator APIs are not available, so simply show
			// and hide the relevant UI components.
			mLoginStatusView.setVisibility(show ? View.VISIBLE : View.GONE);
			mLoginFormView.setVisibility(show ? View.GONE : View.VISIBLE);
		}
	}

	public class UserLoginTask extends AsyncTask<String, Void, Boolean>
	{
		@Override
		protected void onPreExecute()
		{
			mLoginStatusMessageView.setText(R.string.login_login_progress_signing_in);
			showProgress(true);
		}

		@Override
		protected Boolean doInBackground(String... params)
		{
			String email = params[0];
			String password = params[1];

			WebClient client = WebClientInternal.getInstance(ActivityLogin.this, email, password);

			try
			{
				client.login();
				return true;
			}
			catch (Exception e)
			{
				return false;
			}
		}

		@Override
		protected void onPostExecute(final Boolean success)
		{
			mAuthTask = null;

			if (success)
			{
				clearFirstStart();
				addAccount();
				startService(new Intent(ActivityLogin.this, ImportService.class));
			}
			else
			{
				showProgress(false);
				textPassword.setError(getString(R.string.login_error_incorrect_password));
				textPassword.requestFocus();
			}
		}

		@Override
		protected void onCancelled()
		{
			mAuthTask = null;
			showProgress(false);
		}
	}

	private void clearFirstStart()
	{
		PreferencesTypedService preferences = new PreferencesTypedService(new PreferencesLocalService(this));
		preferences.setBooleanValue(PreferenceID.ANDROID_FIRST_START, false);
	}

	private void addAccount()
	{
		String accountName = textEmail.getText().toString();
		String accountPassword = textPassword.getText().toString();
		String accountType = mAccountType;

		Intent intent = new Intent();
		intent.putExtra(AccountManager.KEY_ACCOUNT_NAME, accountName);
		intent.putExtra(AccountManager.KEY_ACCOUNT_TYPE, accountType);
		intent.putExtra(AccountManager.KEY_AUTHTOKEN, "token-goes-here");

		setAccountAuthenticatorResult(intent.getExtras());
		setResult(RESULT_OK, intent);

		AccountManager mAccountManager = AccountManager.get(this);
		Account account = new Account(accountName, accountType);
		if (getIntent().getBooleanExtra(ARG_IS_ADDING_NEW_ACCOUNT, false))
		{
			// String authtoken = intent.getStringExtra(AccountManager.KEY_AUTHTOKEN);
			// Creating the account on the device and setting the auth token we got
			// (Not setting the auth token will cause another call to the server to authenticate the
			// user)
			mAccountManager.addAccountExplicitly(account, accountPassword, null);
			// mAccountManager.setAuthToken(account, mAuthTokenType, authtoken);
		}
		else
		{
			mAccountManager.setPassword(account, accountPassword);
		}
	}

	private final BroadcastReceiver receiver = new BroadcastReceiver()
	{
		@Override
		public void onReceive(Context context, Intent intent)
		{
			Bundle bundle = intent.getExtras();
			if (bundle != null)
			{
				Progress step = (Progress) bundle.getSerializable(ImportService.KEY_RESULT);

				switch (step)
				{
					case INITIALIZATION:
					{
						mLoginStatusMessageView.setText(getString(R.string.login_import_step_initialization));
						break;
					}

					case LOADING:
					{
						mLoginStatusMessageView.setText(getString(R.string.login_import_step_downloading));
						break;
					}

					case UNZIPPING:
					{
						mLoginStatusMessageView.setText(getString(R.string.login_import_step_unzipping));
						break;
					}

					case INSTALL_DIARY:
					{
						mLoginStatusMessageView.setText(getString(R.string.login_import_step_diary));
						break;
					}

					case INSTALL_FOODBASE:
					{
						mLoginStatusMessageView.setText(getString(R.string.login_import_step_food));
						break;
					}

					case INSTALL_DISHBASE:
					{
						mLoginStatusMessageView.setText(getString(R.string.login_import_step_dish));
						break;
					}

					case INSTALL_PREFERENCES:
					{
						mLoginStatusMessageView.setText(getString(R.string.login_import_step_preferences));
						break;
					}

					case DONE_OK:
					case DONE_FAIL:
					{
						if (step == Progress.DONE_OK)
						{
							UIUtils.showTip(ActivityLogin.this, getString(R.string.login_import_result_done));
						}
						else
						{
							UIUtils.showTip(ActivityLogin.this, getString(R.string.login_import_result_fail));
						}

						enableAutosync();

						// TODO: don't do this if activity is started from main activity
						Intent intentR = new Intent(ActivityLogin.this, ActivityMain.class);
						intentR.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP | Intent.FLAG_ACTIVITY_NEW_TASK);
						startActivity(intentR);

						finish();
						break;
					}
				}
			}
		}
	};

	private void enableAutosync()
	{
		Account[] accounts = AccountUtils.getAccounts(this);
		if (accounts.length > 0)
		{
			long SYNC_INTERVAL = 120; // sec
			ContentResolver.setIsSyncable(accounts[0], DiaryContentProvider.AUTHORITY, 1);
			ContentResolver.setSyncAutomatically(accounts[0], DiaryContentProvider.AUTHORITY, true);
			ContentResolver.addPeriodicSync(accounts[0], DiaryContentProvider.AUTHORITY, Bundle.EMPTY, SYNC_INTERVAL);
		}
	}
}
