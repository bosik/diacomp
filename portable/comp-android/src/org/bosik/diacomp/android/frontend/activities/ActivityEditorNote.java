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
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;

public class ActivityEditorNote extends ActivityEditorTime<NoteRecord>
{
	/* =========================== КОНСТАНТЫ ================================ */
	// private static final String TAG = "ActivityEditorNote";

	/* =========================== ПОЛЯ ================================ */

	// компоненты
	private Button				buttonTime;
	private Button				buttonDate;
	private EditText			editText;
	private Button				buttonOK;

	// TODO: localize error message
	private static final String	ERROR_INCORRECT_NOTE_VALUE	= "Ошибка: неверный текст";

	/* =========================== МЕТОДЫ ================================ */

	public ActivityEditorNote()
	{
	}

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.activity_editor_note);

		buttonTime = (Button) findViewById(R.id.buttonNoteTime);
		buttonTime.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				showTimePickerDialog();
			}
		});
		buttonDate = (Button) findViewById(R.id.buttonNoteDate);
		buttonDate.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				showDatePickerDialog();
			}
		});

		editText = (EditText) findViewById(R.id.editNoteText);
		buttonOK = (Button) findViewById(R.id.buttonNoteOK);
		buttonOK.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				ActivityEditorNote.this.submit();
			}
		});
	}

	@Override
	protected void showValuesInGUI(boolean createMode)
	{
		if (!createMode)
		{
			onDateTimeChanged(entity.getData().getTime());
			editText.setText(entity.getData().getText());
		}
		else
		{
			onDateTimeChanged(new Date());
			editText.setText("");
		}
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		// text
		try
		{
			entity.getData().setText(editText.getText().toString());
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(ActivityEditorNote.this, ERROR_INCORRECT_NOTE_VALUE);
			editText.requestFocus();
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
