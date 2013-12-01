package org.bosik.compensation.face;

import android.app.Activity;
import android.widget.Toast;

public class UIUtils
{
	public static void showTip(Activity activity, String Msg)
	{
		Toast.makeText(activity.getApplicationContext(), Msg, Toast.LENGTH_SHORT).show();
	}
}
