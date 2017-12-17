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

import android.annotation.SuppressLint;
import android.app.DatePickerDialog;
import android.app.TimePickerDialog;
import android.os.Bundle;
import android.widget.DatePicker;
import android.widget.TimePicker;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.frontend.fragments.pickers.DatePickerFragment;
import org.bosik.diacomp.android.frontend.fragments.pickers.TimePickerFragment;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.utils.Utils;

import java.util.Date;

// Do not make it abstract: the android.app.Fragment$InstantiationException may be caused otherwise
@SuppressLint("Registered")
public class ActivityEditorTime<T extends DiaryRecord> extends ActivityEditor<T>
		implements DatePickerDialog.OnDateSetListener, TimePickerDialog.OnTimeSetListener
{
	protected void showDatePickerDialog()
	{
		DatePickerFragment newFragment = new DatePickerFragment();
		Bundle args = new Bundle();
		args.putLong(DatePickerFragment.KEY, entity.getData().getTime().getTime());
		newFragment.setArguments(args);
		newFragment.show(getFragmentManager(), "datePicker");
	}

	protected void showTimePickerDialog()
	{
		TimePickerFragment newFragment = new TimePickerFragment();
		Bundle args = new Bundle();
		args.putLong(TimePickerFragment.KEY, entity.getData().getTime().getTime());
		newFragment.setArguments(args);
		newFragment.show(getFragmentManager(), "timePicker");
	}

	protected String formatDate(Date date)
	{
		return UIUtils.formatDateLocalDevice(this, date);
	}

	protected String formatTime(Date time)
	{
		return UIUtils.formatTimeLocalDevice(this, time);
	}

	@Override
	public void onDateSet(DatePicker view, int year, int monthOfYear, int dayOfMonth)
	{
		Date time = Utils.setDate(entity.getData().getTime(), year, monthOfYear, dayOfMonth);
		entity.getData().setTime(time);
		onDateTimeChanged(time);
	}

	@Override
	public void onTimeSet(TimePicker view, int hourOfDay, int minute)
	{
		Date time = Utils.setTime(entity.getData().getTime(), hourOfDay, minute);
		entity.getData().setTime(time);
		onDateTimeChanged(time);
	}

	/**
	 * Called when date or time changed
	 *
	 * @param time New date/time
	 */
	protected void onDateTimeChanged(Date time)
	{
	}
}
