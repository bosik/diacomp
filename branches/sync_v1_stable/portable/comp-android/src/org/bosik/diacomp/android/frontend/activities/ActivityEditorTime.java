package org.bosik.diacomp.android.frontend.activities;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import android.app.DatePickerDialog.OnDateSetListener;
import android.app.TimePickerDialog.OnTimeSetListener;
import android.os.Bundle;
import android.widget.DatePicker;
import android.widget.TimePicker;

public abstract class ActivityEditorTime<T extends DiaryRecord> extends ActivityEditor<T>
{
	private final SimpleDateFormat	FORMAT_TIME	= new SimpleDateFormat("HH:mm", Locale.getDefault());
	private final SimpleDateFormat	FORMAT_DATE	= new SimpleDateFormat("dd/MM/yyyy", Locale.getDefault());

	/* =========================== PROTECTED METHODS ================================ */

	public void showTimePickerDialog()
	{
		TimePickerFragment newFragment = new TimePickerFragment();
		newFragment.setOnTimeSetListener(new OnTimeSetListener()
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
		});
		Bundle args = new Bundle();
		args.putSerializable(TimePickerFragment.FIELD_TIME, entity.getData().getTime());
		newFragment.setArguments(args);
		newFragment.show(getSupportFragmentManager(), "timePicker");
	}

	protected void showDatePickerDialog()
	{
		DatePickerFragment newFragment = new DatePickerFragment();
		newFragment.setOnDateSetListener(new OnDateSetListener()
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
		});
		Bundle args = new Bundle();
		args.putSerializable(DatePickerFragment.FIELD_DATE, entity.getData().getTime());
		newFragment.setArguments(args);
		newFragment.show(getSupportFragmentManager(), "datePicker");
	}

	protected String formatDate(Date date)
	{
		return FORMAT_DATE.format(date);
	}

	protected String formatTime(Date time)
	{
		return FORMAT_TIME.format(time);
	}

	protected abstract void onDateTimeChanged(Date time);
}
