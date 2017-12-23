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

import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.utils.Utils;

import java.util.Date;

public class ActivityEditorIns extends ActivityEditorTime<InsRecord>
{
	// components
	private Button   buttonTime;
	private Button   buttonDate;
	private EditText editValue;
	private Button   buttonOK;

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
	public boolean onCreateOptionsMenu(Menu menu)
	{
		getMenuInflater().inflate(R.menu.actions_editor_ins, menu);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item)
	{
		switch (item.getItemId())
		{
			case R.id.item_ins_help:
			{
				UIUtils.showLongTip(this, getString(R.string.editor_ins_tip));
				return true;
			}
			default:
			{
				return false;
			}
		}
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
			editValue.setText(Utils.formatDoubleShort(entity.getData().getValue()));
		}
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		final String ERROR_INCORRECT_INS_VALUE = getString(R.string.editor_ins_error_invalid_dosage);

		try
		{
			entity.getData().setValue(Utils.parseExpression(editValue.getText().toString()));
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(this, ERROR_INCORRECT_INS_VALUE);
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
