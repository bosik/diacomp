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
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import org.bosik.diacomp.core.utils.Utils;

import java.util.Date;

public class ActivityEditorNote extends ActivityEditorTime<NoteRecord>
{
	// UI
	private Button   buttonTime;
	private Button   buttonDate;
	private EditText editText;

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.activity_editor_note);

		buttonTime = findViewById(R.id.buttonNoteTime);
		buttonTime.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				showTimePickerDialog();
			}
		});
		buttonDate = findViewById(R.id.buttonNoteDate);
		buttonDate.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				showDatePickerDialog();
			}
		});

		editText = findViewById(R.id.editNoteText);
		findViewById(R.id.buttonNoteOK).setOnClickListener(new OnClickListener()
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
		buttonTime.setText(formatTime(entity.getData().getTime()));
		buttonDate.setText(formatDate(entity.getData().getTime()));
		editText.setText(entity.getData().getText());
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		String text = editText.getText().toString();

		if (text != null)
		{
			String cleared = Utils.removeNonUtf8(text);
			if (!text.equals(cleared))
			{
				UIUtils.showTip(this, getString(R.string.common_tip_unsupported_chars_removed));
			}

			entity.getData().setText(cleared);
			return true;
		}
		else
		{
			UIUtils.showTip(this, getString(R.string.editor_note_error_text));
			return false;
		}
	}

	@Override
	protected void onDateTimeChanged(Date time)
	{
		buttonTime.setText(formatTime(time));
		buttonDate.setText(formatDate(time));
	}
}
