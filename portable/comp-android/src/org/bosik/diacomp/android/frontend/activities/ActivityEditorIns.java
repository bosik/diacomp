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
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;

public class ActivityEditorIns extends ActivityEditorTime<InsRecord>
{
	// components
	private Button		buttonTime;
	private Button		buttonDate;
	private EditText	editValue;
	private Button		buttonOK;

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.activity_editor_ins);
		buttonTime = (Button) findViewById(R.id.buttonInsTime);
		buttonTime.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				showTimePickerDialog();
			}
		});
		buttonDate = (Button) findViewById(R.id.buttonInsDate);
		buttonDate.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				showDatePickerDialog();
			}
		});
		editValue = (EditText) findViewById(R.id.editInsValue);
		buttonOK = (Button) findViewById(R.id.buttonInsOK);
		buttonOK.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				ActivityEditorIns.this.submit();
			}
		});
	}

	@Override
	protected void showValuesInGUI(boolean createMode)
	{
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
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		final String ERROR_INCORECT_INS_VALUE = getString(R.string.editor_ins_error_invalid_dosage);

		try
		{
			entity.getData().setValue(Double.parseDouble(editValue.getText().toString()));
		}
		catch (NumberFormatException e)
		{
			UIUtils.showTip(this, ERROR_INCORECT_INS_VALUE);
			editValue.requestFocus();
			return false;
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(this, ERROR_INCORECT_INS_VALUE);
			editValue.requestFocus();
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
