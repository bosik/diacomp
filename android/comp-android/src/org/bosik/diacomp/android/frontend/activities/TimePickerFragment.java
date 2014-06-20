package org.bosik.diacomp.android.frontend.activities;

import java.util.Calendar;
import java.util.Date;
import android.app.Dialog;
import android.app.DialogFragment;
import android.app.TimePickerDialog;
import android.os.Bundle;
import android.text.format.DateFormat;

public abstract class TimePickerFragment extends DialogFragment implements TimePickerDialog.OnTimeSetListener
{
	private final Date	time;

	public TimePickerFragment(Date time)
	{
		this.time = time;
	}

	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState)
	{
		Calendar c = Calendar.getInstance();
		c.setTime(time);
		int hour = c.get(Calendar.HOUR_OF_DAY);
		int minute = c.get(Calendar.MINUTE);

		return new TimePickerDialog(getActivity(), this, hour, minute, DateFormat.is24HourFormat(getActivity()));
	}
}