package org.bosik.diacomp.android.frontend.activities;

import org.bosik.diacomp.android.R;
import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

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
				startActivity(intent);
			}
		});

		Button buttonSetup = (Button) findViewById(R.id.buttonWelcomeSetup);
		buttonSetup.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				// TODO
				Intent intent = new Intent(ActivityWelcome.this, ActivityFoodSet.class);
				intent.putExtra(ActivityFoodSet.FIELD_FIRST_START, true);
				startActivity(intent);
			}
		});
	}
}
