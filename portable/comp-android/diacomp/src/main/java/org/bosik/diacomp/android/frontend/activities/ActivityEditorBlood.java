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
import org.bosik.diacomp.core.entities.business.BloodSugarUnit;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.utils.Utils;

import java.util.Date;
import java.util.Locale;

public class ActivityEditorBlood extends ActivityEditorTime<BloodRecord>
{
	// components
	private EditText editValue;
	private TextView labelBloodFinger;
	private Spinner  spinnerUnit;
	private Spinner  spinnerFinger;
	private Button   buttonTime;
	private Button   buttonDate;

	// TODO: i18n
	private static final String ERROR_INCORRECT_FINGER_VALUE = "Укажите палец, из которого бралась кровь";
	private static final String ERROR_INCORRECT_UNIT         = "Incorrect unit of measurement";

	// parameters
	private final boolean askFinger = true;

	/* =========================== OVERRIDDEN METHODS ================================ */

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.activity_editor_blood);
		editValue = findViewById(R.id.editBloodValue);
		spinnerUnit = findViewById(R.id.spinnerUnit);
		labelBloodFinger = findViewById(R.id.labelBloodFinger);
		spinnerFinger = findViewById(R.id.spinnerBloodFinger);

		buttonTime = findViewById(R.id.buttonBloodTime);
		buttonTime.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				showTimePickerDialog();
			}
		});
		buttonDate = findViewById(R.id.buttonBloodDate);
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

		final BloodSugarUnit unit = createMode
				? BloodSugarUnit.MG_DL // FIXME: use preferences (last used unit)
				: entity.getData().getUnit();

		spinnerUnit.setSelection(writeUnit(unit));

		if (entity.getData().getValue() == 0)
		{
			editValue.setText("");
		}
		else
		{
			editValue.setText(unit == BloodSugarUnit.MMOL_L
					? String.format(Locale.US, "%.1f", entity.getData().getValue())
					: String.format(Locale.US, "%.0f", entity.getData().getValue())
			);
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

		// unit
		try
		{
			entity.getData().setUnit(readUnit(spinnerUnit.getSelectedItemPosition()));
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(this, ERROR_INCORRECT_UNIT);
			spinnerUnit.requestFocus();
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

	private static int writeUnit(BloodSugarUnit unit)
	{
		return unit == BloodSugarUnit.MMOL_L
				? 0
				: 1;
	}

	private static BloodSugarUnit readUnit(int index)
	{
		switch (index)
		{
			case 0:
				return BloodSugarUnit.MMOL_L;
			case 1:
				return BloodSugarUnit.MG_DL;
			default:
				throw new IllegalArgumentException();
		}
	}
}
