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
package org.bosik.diacomp.android.frontend.views.diary;

import org.bosik.diacomp.android.R;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.LinearLayout;
import android.widget.TextView;

public class DiaryRecNoteView extends LinearLayout
{
	// Data
	private Versioned<NoteRecord>	record;

	// Components
	private TextView				textTime;
	private TextView				textValue;

	public DiaryRecNoteView(Context context)
	{
		super(context);
		LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.view_diary_rec_note, this);

		textTime = (TextView) findViewById(R.id.textNoteTime);
		textValue = (TextView) findViewById(R.id.textNoteValue);
	}

	public void setData(Versioned<NoteRecord> record)
	{
		this.record = record;
		NoteRecord data = record.getData();

		textTime.setText(Utils.formatTimeLocalShort(data.getTime()));
		textValue.setText(data.getText());
	}
}
