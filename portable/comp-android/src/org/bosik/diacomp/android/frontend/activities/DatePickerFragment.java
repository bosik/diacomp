package org.bosik.diacomp.android.frontend.activities;

import java.util.Calendar;
import java.util.Date;
import android.app.DatePickerDialog;
import android.app.DatePickerDialog.OnDateSetListener;
import android.app.Dialog;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;

public class DatePickerFragment extends DialogFragment
{
	public static final String	FIELD_DATE	= "date";

	private Date				date;
	private OnDateSetListener	onDateSet;

	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState)
	{
		Bundle args = getArguments();

		if (args != null && args.containsKey(FIELD_DATE))
		{
			date = (Date) args.getSerializable(FIELD_DATE);
		}
		else
		{
			date = new Date();
		}

		Calendar c = Calendar.getInstance();
		c.setTime(date);
		int year = c.get(Calendar.YEAR);
		int month = c.get(Calendar.MONTH);
		int day = c.get(Calendar.DAY_OF_MONTH);

		return new DatePickerDialog(getActivity(), onDateSet, year, month, day);
	}

	public DatePickerFragment setOnDateSetListener(OnDateSetListener l)
	{
		onDateSet = l;
		return this;
	}
}