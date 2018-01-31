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
package org.bosik.diacomp.android.frontend;

import java.util.Date;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.core.utils.Utils;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.AlertDialog.Builder;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnCancelListener;
import android.text.InputType;
import android.text.format.DateFormat;
import android.text.method.DigitsKeyListener;
import android.widget.EditText;
import android.widget.Toast;

public class UIUtils
{
	public static void showTip(Context context, String msg)
	{
		Toast.makeText(context, msg, Toast.LENGTH_SHORT).show();
	}

	public static void showTip(Activity activity, String msg)
	{
		showTip(activity.getApplicationContext(), msg);
	}

	public static void showTipLong(Context context, String msg)
	{
		Toast.makeText(context, msg, Toast.LENGTH_LONG).show();
	}

	public static void showLongTip(Activity activity, String msg)
	{
		showTipLong(activity.getApplicationContext(), msg);
	}

	public interface OnSubmit
	{
		void onSubmit(Double mass);

		void onCancel();
	}

	public static void requestMass(final Context context, String title, String message, String defaultMass,
			final OnSubmit e)
	{
		Builder builder = new AlertDialog.Builder(context);

		final EditText input = new EditText(context);

		input.setText(defaultMass);
		input.setInputType(InputType.TYPE_NUMBER_FLAG_DECIMAL | InputType.TYPE_CLASS_NUMBER);
		input.setKeyListener(DigitsKeyListener.getInstance("0123456789.,+-"));
		builder.setTitle(title);
		builder.setMessage(message);
		builder.setView(input);

		builder.setPositiveButton(context.getString(R.string.common_button_ok), new DialogInterface.OnClickListener()
		{
			@Override
			public void onClick(DialogInterface dialog, int whichButton)
			{
				String text = input.getText().toString();

				try
				{
					if (text.isEmpty())
					{
						e.onSubmit(null);
					}
					else
					{
						double mass = Utils.parseExpression(text);
						if (mass > Utils.EPS)
						{
							e.onSubmit(mass);
						}
						else
						{
							e.onSubmit(null);
						}
					}
				}
				catch (NumberFormatException ex)
				{
					UIUtils.showTip((Activity) context, context.getString(R.string.editor_mass_error));
					e.onCancel();
				}
			}
		});
		builder.setOnCancelListener(new OnCancelListener()
		{
			@Override
			public void onCancel(DialogInterface dialog)
			{
				e.onCancel();
			}
		});
		builder.setNegativeButton(context.getString(R.string.common_button_cancel),
				new DialogInterface.OnClickListener()
				{
					@Override
					public void onClick(DialogInterface dialog, int whichButton)
					{
						e.onCancel();
					}
				});
		builder.show();
	}

	/**
	 * Formats date using local device format
	 * 
	 * @param context
	 * @param date
	 * @return
	 */
	public static String formatDateLocalDevice(Context context, Date date)
	{
		if (date == null)
		{
			throw new IllegalArgumentException("date is null");
		}

		return DateFormat.getMediumDateFormat(context).format(date);
	}

	/**
	 * Formats time using local device format
	 * 
	 * @param context
	 * @param time
	 * @return
	 */
	public static String formatTimeLocalDevice(Context context, Date time)
	{
		if (time == null)
		{
			throw new IllegalArgumentException("time is null");
		}

		return DateFormat.getTimeFormat(context).format(time);
	}
}
