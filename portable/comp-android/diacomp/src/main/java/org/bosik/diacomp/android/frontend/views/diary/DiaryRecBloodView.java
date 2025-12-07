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
import org.bosik.diacomp.android.backend.features.preferences.account.PreferencesLocalService;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.Units;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.services.diary.MealFormat;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.services.preferences.PreferencesTypedService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

import java.util.TimeZone;

public class DiaryRecBloodView extends LinearLayout
{
	private final TextView textTime;
	private final TextView textValue;

	private final PreferencesTypedService preferences;

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

		textTime = findViewById(R.id.textBloodTime);
		textValue = findViewById(R.id.textBloodValue);
		preferences = new PreferencesTypedService(new PreferencesLocalService(context));

		setData(record);
	}

	public void setData(Versioned<BloodRecord> record)
	{
		BloodRecord data = record.getData();

		textTime.setText(Utils.formatTimeLocalShort(TimeZone.getDefault(), data.getTime()));

		if (isInEditMode())
		{
			textValue.setText(UIUtils.formatBloodSugar(getContext(), 5.2, Units.BloodSugar.MMOL_L));
		}
		else
		{
			final Units.BloodSugar unit = preferences.getEnum(PreferenceID.BLOOD_SUGAR_UNITS, Units.BloodSugar.class);
			final String bloodSugar = UIUtils.formatBloodSugar(getContext(), data.getValue(unit), unit);
			final String finger = data.getFinger() != -1
					? String.format("(%s)", getResources().getStringArray(R.array.fingers_short)[data.getFinger()])
					: "";

			textValue.setText(bloodSugar + " " + finger);
		}
	}
}
