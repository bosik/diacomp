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

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import org.bosik.diacomp.android.frontend.fragments.DatePickerFragment;
import org.bosik.diacomp.android.frontend.fragments.TimePickerFragment;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import android.app.DialogFragment;
import android.widget.DatePicker;
import android.widget.TimePicker;

// Do not make it abstract: the android.app.Fragment$InstantiationException may be caused otherwise
public class ActivityEditorTime<T extends DiaryRecord> extends ActivityEditor<T>
{
	private final SimpleDateFormat	FORMAT_TIME	= new SimpleDateFormat("HH:mm", Locale.getDefault());
	private final SimpleDateFormat	FORMAT_DATE	= new SimpleDateFormat("dd/MM/yyyy", Locale.getDefault());

	/* =========================== PROTECTED METHODS ================================ */

	protected void showTimePickerDialog()
	{
		DialogFragment newFragment = new TimePickerFragment(entity.getData().getTime())
		{
			@Override
			public void onTimeSet(TimePicker view, int hourOfDay, int minute)
			{
				Calendar c = Calendar.getInstance();
				c.setTime(entity.getData().getTime());

				c.set(Calendar.HOUR_OF_DAY, hourOfDay);
				c.set(Calendar.MINUTE, minute);

				entity.getData().setTime(c.getTime());
				onDateTimeChanged(c.getTime());
			}
		};
		newFragment.show(getFragmentManager(), "timePicker");
	}

	protected void showDatePickerDialog()
	{
		DialogFragment newFragment = new DatePickerFragment(entity.getData().getTime())
		{
			@Override
			public void onDateSet(DatePicker view, int year, int monthOfYear, int dayOfMonth)
			{
				Calendar c = Calendar.getInstance();
				c.setTime(entity.getData().getTime());

				c.set(Calendar.YEAR, year);
				c.set(Calendar.MONTH, monthOfYear);
				c.set(Calendar.DAY_OF_MONTH, dayOfMonth);

				entity.getData().setTime(c.getTime());
				onDateTimeChanged(c.getTime());
			}
		};
		newFragment.show(getFragmentManager(), "datePicker");
	}

	protected String formatDate(Date date)
	{
		return FORMAT_DATE.format(date);
	}

	protected String formatTime(Date time)
	{
		return FORMAT_TIME.format(time);
	}

	/**
	 * Called when date or time changed
	 * 
	 * @param time
	 *            New date/time
	 */
	protected void onDateTimeChanged(Date time)
	{
	};
}
