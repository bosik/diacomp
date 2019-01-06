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
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.services.diary.MealFormat;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.services.preferences.PreferencesTypedService;
import org.bosik.diacomp.core.utils.CodedUtils;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.Versioned;

import java.util.TimeZone;

public class DiaryRecMealView extends LinearLayout
{
	private static final MealFormat DEFAULT_FORMAT = CodedUtils.parse(MealFormat.class, PreferenceID.ANDROID_MEAL_FORMAT.getDefaultValue());

	private TextView textTime;
	private TextView textValue;

	private PreferencesTypedService preferences;

	public DiaryRecMealView(Context context)
	{
		super(context);
		LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.view_diary_rec_meal, this);

		textTime = (TextView) findViewById(R.id.textMealTime);
		textValue = (TextView) findViewById(R.id.textMealValue);

		preferences = new PreferencesTypedService(new PreferencesLocalService(context));
	}

	public void setData(Versioned<MealRecord> record)
	{
		String s = preferences.getStringValue(PreferenceID.ANDROID_MEAL_FORMAT);
		final MealFormat mealFormat = CodedUtils.parse(MealFormat.class, s, DEFAULT_FORMAT);

		MealRecord data = record.getData();
		textTime.setText(Utils.formatTimeLocalShort(TimeZone.getDefault(), data.getTime()));
		final String tmp = MealFormatter.format(data, getContext(), mealFormat);
		textValue.setText(tmp);
	}
}
