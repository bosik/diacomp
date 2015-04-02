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

import org.bosik.diacomp.core.utils.Utils;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.AlertDialog.Builder;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnCancelListener;
import android.text.InputType;
import android.widget.EditText;
import android.widget.Toast;

public class UIUtils
{
	public static void showTip(Activity activity, String Msg)
	{
		Toast.makeText(activity.getApplicationContext(), Msg, Toast.LENGTH_SHORT).show();
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
		input.setInputType(InputType.TYPE_NUMBER_FLAG_DECIMAL);
		// TODO: localize
		builder.setTitle(title);
		builder.setMessage(message);
		builder.setView(input);
		builder.setPositiveButton("Ok", new DialogInterface.OnClickListener()
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
					// TODO: localize
					UIUtils.showTip((Activity) context, "Wrong mass");
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
		builder.setNegativeButton("Cancel", new DialogInterface.OnClickListener()
		{
			@Override
			public void onClick(DialogInterface dialog, int whichButton)
			{
				e.onCancel();
			}
		});
		builder.show();
	}
}
