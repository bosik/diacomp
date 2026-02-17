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

import android.view.View;
import android.view.inputmethod.EditorInfo;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Spinner;
import android.widget.TextView;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.preferences.account.PreferencesLocalService;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.Units;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.services.preferences.PreferencesTypedService;
import org.bosik.diacomp.core.utils.Utils;

import java.util.Date;

public class ActivityEditorBlood extends ActivityEditorTime<BloodRecord>
{
	// components
	private EditText editValue;
	private TextView labelBloodFinger;
	private Spinner  spinnerUnit;
	private Spinner  spinnerFinger;
	private Button   buttonTime;
	private Button   buttonDate;

	private PreferencesTypedService preferences;

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
		buttonTime.setOnClickListener(v -> showTimePickerDialog());
		buttonDate = findViewById(R.id.buttonBloodDate);
		buttonDate.setOnClickListener(v -> showDatePickerDialog());

		editValue.setOnEditorActionListener((v, actionId, event) -> {
			if (actionId == EditorInfo.IME_ACTION_DONE)
			{
				submit();
				return true;
			}
			return false;
		});

		findViewById(R.id.buttonBloodOK).setOnClickListener(v -> ActivityEditorBlood.this.submit());

		preferences = new PreferencesTypedService(new PreferencesLocalService(this));
	}

	@Override
	protected void showValuesInGUI(boolean createMode)
	{
		buttonDate.setText(formatDate(entity.getData().getTime()));
		buttonTime.setText(formatTime(entity.getData().getTime()));

		final Units.BloodSugar unit = createMode
				? preferences.getEnum(PreferenceID.ANDROID_LAST_USED_BLOOD_SUGAR_UNITS, Units.BloodSugar.class)
				: entity.getData().getUnit();

		spinnerUnit.setSelection(writeUnit(unit));

		if (entity.getData().getValue() == 0)
		{
			editValue.setText("");
		}
		else
		{
			editValue.setText(UIUtils.formatBloodSugarValue(entity.getData().getValue(), unit));
		}

		if (askFinger)
		{
			spinnerFinger.setSelection(entity.getData().getFinger() + 1);
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
			UIUtils.showTip(this, getString(R.string.editor_blood_error_invalid_bs_unit));
			spinnerUnit.requestFocus();
			return false;
		}

		// finger
		try
		{
			if (askFinger)
			{
				entity.getData().setFinger(spinnerFinger.getSelectedItemPosition() - 1);
			}
			else
			{
				entity.getData().setFinger(-1);
			}
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(this, getString(R.string.editor_blood_error_invalid_finger));
			spinnerFinger.requestFocus();
			return false;
		}

		preferences.setEnum(PreferenceID.ANDROID_LAST_USED_BLOOD_SUGAR_UNITS, entity.getData().getUnit());

		return true;
	}

	@Override
	protected void onDateTimeChanged(Date time)
	{
		buttonTime.setText(formatTime(time));
		buttonDate.setText(formatDate(time));
	}

	private static int writeUnit(Units.BloodSugar unit)
	{
		return unit == Units.BloodSugar.MMOL_L
				? 0
				: 1;
	}

	private static Units.BloodSugar readUnit(int index)
	{
		return switch (index)
		{
			case 0 -> Units.BloodSugar.MMOL_L;
			case 1 -> Units.BloodSugar.MG_DL;
			default -> throw new IllegalArgumentException();
		};
	}
}
