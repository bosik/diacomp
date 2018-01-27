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
package org.bosik.diacomp.android.frontend.views;

import android.annotation.TargetApi;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.os.Build;
import android.os.Bundle;
import android.preference.EditTextPreference;
import android.util.AttributeSet;
import android.view.View;
import android.widget.Button;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.core.utils.Utils;

import java.text.ParseException;

public class MandatoryDecimalPreference extends EditTextPreference
{
	@TargetApi(Build.VERSION_CODES.LOLLIPOP)
	public MandatoryDecimalPreference(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes)
	{
		super(context, attrs, defStyleAttr, defStyleRes);
	}

	public MandatoryDecimalPreference(Context context, AttributeSet attrs, int defStyleAttr)
	{
		super(context, attrs, defStyleAttr);
	}

	public MandatoryDecimalPreference(Context context, AttributeSet attrs)
	{
		super(context, attrs);
	}

	public MandatoryDecimalPreference(Context context)
	{
		super(context);
	}

	@Override
	protected void showDialog(Bundle state)
	{
		super.showDialog(state);
		final AlertDialog dialog = (AlertDialog) getDialog();
		Button buttonOk = dialog.getButton(DialogInterface.BUTTON_POSITIVE);
		buttonOk.setOnClickListener(new View.OnClickListener()
		{
			@Override
			public void onClick(View view)
			{
				String value = getEditText().getText().toString();
				try
				{
					Utils.parseDouble(value);
					MandatoryDecimalPreference.super.onClick(dialog, DialogInterface.BUTTON_POSITIVE);
					dialog.dismiss();
				}
				catch (ParseException e)
				{
					getEditText().setError(getContext().getString(R.string.common_tip_error_invalid_decimal));
				}
			}
		});

		getEditText().setError(null);
	}
}
