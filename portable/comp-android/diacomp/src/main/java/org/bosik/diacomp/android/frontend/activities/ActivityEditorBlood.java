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

import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.inputmethod.EditorInfo;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Spinner;
import android.widget.TextView;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.utils.Utils;

import java.util.Date;

public class ActivityEditorBlood extends ActivityEditorTime<BloodRecord>
{
	// components
	private EditText editValue;
	private TextView labelBloodFinger;
	private Spinner  spinnerFinger;
	private Button   buttonTime;
	private Button   buttonDate;

	// TODO: i18n
	private static final String ERROR_INCORRECT_FINGER_VALUE = "Укажите палец, из которого бралась кровь";

	// parameters
	private final boolean askFinger = true;

	/* =========================== OVERRIDDEN METHODS ================================ */

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.activity_editor_blood);
		editValue = (EditText) findViewById(R.id.editBloodValue);
		labelBloodFinger = (TextView) findViewById(R.id.labelBloodFinger);
		spinnerFinger = (Spinner) findViewById(R.id.spinnerBloodFinger);

		buttonTime = (Button) findViewById(R.id.buttonBloodTime);
		buttonTime.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				showTimePickerDialog();
			}
		});
		buttonDate = (Button) findViewById(R.id.buttonBloodDate);
		buttonDate.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				showDatePickerDialog();
			}
		});

		editValue.setOnEditorActionListener(new TextView.OnEditorActionListener()
		{
			@Override
			public boolean onEditorAction(TextView v, int actionId, KeyEvent event)
			{
				if (actionId == EditorInfo.IME_ACTION_DONE)
				{
					submit();
					return true;
				}
				return false;
			}
		});

		findViewById(R.id.buttonBloodOK).setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				ActivityEditorBlood.this.submit();
			}
		});
	}

	@Override
	protected void showValuesInGUI(boolean createMode)
	{
		buttonDate.setText(formatDate(entity.getData().getTime()));
		buttonTime.setText(formatTime(entity.getData().getTime()));

		if (entity.getData().getValue() == 0)
		{
			editValue.setText("");
		}
		else
		{
			editValue.setText(String.valueOf(entity.getData().getValue()));
		}

		if (askFinger)
		{
			spinnerFinger.setSelection(entity.getData().getFinger());
		}
		else
		{
			spinnerFinger.setVisibility(View.GONE);
			labelBloodFinger.setVisibility(View.GONE);
		}
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		// value
		try
		{
			double value = Utils.parseExpression(editValue.getText().toString());
			if (value <= 0)
			{
				throw new IllegalArgumentException();
			}

			entity.getData().setValue(value);
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(this, getString(R.string.editor_blood_error_invalid_bs));
			editValue.requestFocus();
			return false;
		}

		// finger
		try
		{
			if (askFinger)
			{
				entity.getData().setFinger(spinnerFinger.getSelectedItemPosition());
			}
			else
			{
				entity.getData().setFinger(-1);
			}
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(this, ERROR_INCORRECT_FINGER_VALUE);
			spinnerFinger.requestFocus();
			return false;
		}

		return true;
	}

	@Override
	protected void onDateTimeChanged(Date time)
	{
		buttonTime.setText(formatTime(time));
		buttonDate.setText(formatDate(time));
	}
}
