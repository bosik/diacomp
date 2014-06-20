package org.bosik.diacomp.android.frontend.activities;

import java.util.Calendar;
import java.util.Date;
import android.app.DatePickerDialog;
import android.app.Dialog;
import android.app.DialogFragment;
import android.os.Bundle;

public abstract class DatePickerFragment extends DialogFragment implements DatePickerDialog.OnDateSetListener
{
	private final Date	time;

	public DatePickerFragment(Date time)
	{
		this.time = time;
	}

	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState)
	{
		Calendar c = Calendar.getInstance();
		c.setTime(time);
		int year = c.get(Calendar.YEAR);
		int month = c.get(Calendar.MONTH);
		int day = c.get(Calendar.DAY_OF_MONTH);

		return new DatePickerDialog(getActivity(), this, year, month, day);
	}
}