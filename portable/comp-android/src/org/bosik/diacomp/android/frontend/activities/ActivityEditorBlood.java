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

import java.util.Date;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Spinner;
import android.widget.TextView;

public class ActivityEditorBlood extends ActivityEditorTime<BloodRecord>
{
	// private static final String TAG = ActivityEditorBlood.class.getSimpleName();

	// components
	private EditText			editValue;
	private TextView			labelBloodFinger;
	private Spinner				spinnerFinger;
	private Button				buttonTime;
	private Button				buttonDate;
	private Button				buttonOK;

	// TODO: localize error messages
	private static final String	ERROR_INCORRECT_BS_VALUE		= "Введите корректное значение СК";
	private static final String	ERROR_INCORRECT_FINGER_VALUE	= "Укажите палец, из которого бралась кровь";

	// parameters
	private final boolean		askFinger						= true;

	/* =========================== OVERRIDEN METHODS ================================ */

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

		buttonOK = (Button) findViewById(R.id.buttonBloodOK);
		buttonOK.setOnClickListener(new OnClickListener()
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
		spinnerFinger.setSelection(entity.getData().getFinger());

		if (!createMode)
		{
			onDateTimeChanged(entity.getData().getTime());
			editValue.setText(String.valueOf(entity.getData().getValue()));
		}
		else
		{
			onDateTimeChanged(new Date());
			editValue.setText("");
		}

		if (!askFinger)
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
			entity.getData().setValue(Double.parseDouble(editValue.getText().toString()));
		}
		catch (NumberFormatException e)
		{
			UIUtils.showTip(this, ERROR_INCORRECT_BS_VALUE);
			editValue.requestFocus();
			return false;
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(this, ERROR_INCORRECT_BS_VALUE);
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
