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

import android.content.Context;
import android.view.LayoutInflater;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

import java.util.Locale;
import java.util.TimeZone;

public class DiaryRecBloodView extends LinearLayout
{
	// Components
	private final TextView textTime;
	private final TextView textValue;

	public DiaryRecBloodView(Context context, Versioned<BloodRecord> record)
	{
		super(context);
		LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);

		if (record.getData().isPostPrand())
		{
			inflater.inflate(R.layout.view_diary_rec_blood_postprand, this);
		}
		else
		{
			inflater.inflate(R.layout.view_diary_rec_blood_std, this);
		}

		textTime = (TextView) findViewById(R.id.textBloodTime);
		textValue = (TextView) findViewById(R.id.textBloodValue);

		setData(record);
	}

	public void setData(Versioned<BloodRecord> record)
	{
		BloodRecord data = record.getData();

		textTime.setText(Utils.formatTimeLocalShort(TimeZone.getDefault(), data.getTime()));

		String units = getContext().getString(R.string.common_unit_bs_mmoll);

		if (isInEditMode())
		{
			String text = String.format(Locale.US, "%.1f %s", 5.2, units);
			textValue.setText(text);
		}
		else
		{
			final String[] fingers = getResources().getStringArray(R.array.fingers_short);
			String finger = data.getFinger() == -1 ? "" : String.format("(%s)", fingers[data.getFinger()]);
			String text = String.format(Locale.US, "%.1f %s %s", data.getValue(), units, finger);

			textValue.setText(text);
		}
	}
}
