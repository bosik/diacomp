package org.bosik.diacomp.android.backend.features.notifications;

import org.bosik.diacomp.android.backend.features.analyze.BackgroundService;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class BootCompletedIntentReceiver extends BroadcastReceiver
{
	@Override
	public void onReceive(Context context, Intent intent)
	{
		if ("android.intent.action.BOOT_COMPLETED".equals(intent.getAction()))
		{
			context.startService(new Intent(context, NotificationService.class));
			context.startService(new Intent(context, BackgroundService.class));
		}
	}
}
