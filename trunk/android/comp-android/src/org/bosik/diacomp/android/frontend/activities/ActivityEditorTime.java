package org.bosik.diacomp.android.frontend.activities;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import android.app.DialogFragment;
import android.widget.DatePicker;
import android.widget.TimePicker;

public abstract class ActivityEditorTime<T extends DiaryRecord> extends ActivityEditor<T>
{
	private static final SimpleDateFormat	FORMAT_TIME	= new SimpleDateFormat("HH:mm", Locale.getDefault());
	private static final SimpleDateFormat	FORMAT_DATE	= new SimpleDateFormat("dd/MM/yyyy", Locale.getDefault());

	/* =========================== PROTECTED METHODS ================================ */

	@Deprecated
	protected static Date readTime(DatePicker datePicker, TimePicker timePicker)
	{
		final int year = datePicker.getYear();
		final int month = datePicker.getMonth();
		final int day = datePicker.getDayOfMonth();
		final Integer hour = timePicker.getCurrentHour();
		final Integer minute = timePicker.getCurrentMinute();
		Calendar time = new GregorianCalendar(year, month, day, hour, minute);

		return time.getTime();
	}

	@Deprecated
	protected static void showTime(Date time, DatePicker datePicker, TimePicker timePicker)
	{
		Calendar c = Calendar.getInstance();
		c.setTime(time);

		final int year = c.get(Calendar.YEAR);
		final int month = c.get(Calendar.MONTH);
		final int day = c.get(Calendar.DAY_OF_MONTH);

		datePicker.updateDate(year, month, day);
		timePicker.setCurrentHour(c.get(Calendar.HOUR_OF_DAY));
		timePicker.setCurrentMinute(c.get(Calendar.MINUTE));
	}

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

	protected static String formatDate(Date date)
	{
		return FORMAT_DATE.format(date);
	}

	protected static String formatTime(Date time)
	{
		return FORMAT_TIME.format(time);
	}

	protected abstract void onDateTimeChanged(Date time);
}
