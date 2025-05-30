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
package org.bosik.diacomp.android.frontend.fragments.pickers;

import android.app.Dialog;
import android.app.DialogFragment;
import android.app.TimePickerDialog;
import android.os.Bundle;
import android.text.format.DateFormat;

import java.util.Calendar;
import java.util.Date;

/**
 * Parent activity must implement TimePickerDialog.OnTimeSetListener
 */
public class TimePickerFragment extends DialogFragment
{
	public static final String KEY = "time";

	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		Date time;
		if (getArguments().containsKey(KEY))
		{
			time = new Date(getArguments().getLong(KEY));
		}
		else
		{
			time = new Date();
		}

		Calendar c = Calendar.getInstance();
		c.setTime(time);
		int hour = c.get(Calendar.HOUR_OF_DAY);
		int minute = c.get(Calendar.MINUTE);

		return new TimePickerDialog(getActivity(), (TimePickerDialog.OnTimeSetListener) getActivity(), hour, minute,
				DateFormat.is24HourFormat(getActivity()));
	}
}