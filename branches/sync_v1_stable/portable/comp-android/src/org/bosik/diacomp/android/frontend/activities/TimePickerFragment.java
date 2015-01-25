package org.bosik.diacomp.android.frontend.activities;

import java.util.Calendar;
import java.util.Date;
import android.app.Dialog;
import android.app.TimePickerDialog;
import android.app.TimePickerDialog.OnTimeSetListener;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;
import android.text.format.DateFormat;

public class TimePickerFragment extends DialogFragment
{
	public static final String	FIELD_TIME	= "time";

	private Date				time;
	private OnTimeSetListener	onTimeSet;

	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState)
	{
		Bundle args = getArguments();

		if (args != null && args.containsKey(FIELD_TIME))
		{
			time = (Date) args.getSerializable(FIELD_TIME);
		}
		else
		{
			time = new Date();
		}

		Calendar c = Calendar.getInstance();
		c.setTime(time);
		int hour = c.get(Calendar.HOUR_OF_DAY);
		int minute = c.get(Calendar.MINUTE);

		return new TimePickerDialog(getActivity(), onTimeSet, hour, minute, DateFormat.is24HourFormat(getActivity()));
	}

	public TimePickerFragment setOnTimeSetListener(OnTimeSetListener l)
	{
		onTimeSet = l;
		return this;
	}
}