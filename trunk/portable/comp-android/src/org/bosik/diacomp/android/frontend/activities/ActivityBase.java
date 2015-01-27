package org.bosik.diacomp.android.frontend.activities;

import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.utils.ErrorHandler;
import android.app.Activity;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;

public class ActivityBase extends Activity
{
	private static final String	TAG	= ActivityBase.class.getSimpleName();

	// ===========================================================================

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_base);
	}

	// handled
	@Override
	public boolean onCreateOptionsMenu(Menu menu)
	{
		try
		{
			MenuInflater inflater = getMenuInflater();
			inflater.inflate(R.menu.activity_base_actions, menu);
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
		return true;
	}
}
