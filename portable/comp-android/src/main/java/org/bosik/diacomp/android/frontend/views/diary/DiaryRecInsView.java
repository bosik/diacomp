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

import java.util.TimeZone;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.LinearLayout;
import android.widget.TextView;

public class DiaryRecInsView extends LinearLayout
{
	private TextView	textTime;
	private TextView	textValue;

	public DiaryRecInsView(Context context)
	{
		super(context);
		LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.view_diary_rec_ins, this);

		textTime = (TextView) findViewById(R.id.textInsTime);
		textValue = (TextView) findViewById(R.id.textInsValue);
	}

	public void setData(Versioned<InsRecord> record)
	{
		InsRecord data = record.getData();

		textTime.setText(Utils.formatTimeLocalShort(TimeZone.getDefault(), data.getTime()));

		String units = getContext().getString(R.string.common_unit_insulin);
		String text = Utils.formatDoubleShort(data.getValue()) + " " + units;

		textValue.setText(text);
	}
}
