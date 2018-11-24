package org.bosik.diacomp.android.frontend.activities;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.preferences.account.PreferencesLocalService;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.services.preferences.PreferencesTypedService;

public class ActivityWelcome extends Activity
{
	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_welcome);

		Button buttonSignIn = (Button) findViewById(R.id.buttonWelcomeSignIn);
		buttonSignIn.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				Intent intent = new Intent(ActivityWelcome.this, ActivityLogin.class);
				intent.putExtra(ActivityLogin.ARG_ACCOUNT_TYPE, ActivityLogin.DEFAULT_ACCOUNT_TYPE);
				intent.putExtra(ActivityLogin.ARG_AUTH_TYPE, (String) null);
				intent.putExtra(ActivityLogin.ARG_IS_ADDING_NEW_ACCOUNT, true);
				startActivity(intent);

				// don't clear first start here (e.g. user can return back from ActivityLogin without logging)
			}
		});

		Button buttonSetup = (Button) findViewById(R.id.buttonWelcomeSetup);
		buttonSetup.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				// TODO: implement proper survey

				clearFirstStart();
				startActivity(new Intent(ActivityWelcome.this, ActivityMain.class));
			}
		});
	}

	private void clearFirstStart()
	{
		PreferencesTypedService preferences = new PreferencesTypedService(new PreferencesLocalService(this));
		preferences.setBooleanValue(PreferenceID.ANDROID_FIRST_START, false);
	}
}
