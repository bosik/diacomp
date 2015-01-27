package org.bosik.diacomp.android.frontend.activities;

import org.bosik.diacomp.android.R;
import android.app.Activity;
import android.os.Bundle;

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
}
