package org.bosik.diacomp.android.frontend.activities;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import android.app.Dialog;
import android.app.DialogFragment;
import android.app.TimePickerDialog;
import android.os.Bundle;
import android.text.format.DateFormat;
import android.widget.DatePicker;
import android.widget.TimePicker;

public abstract class ActivityEditorTime<T extends DiaryRecord> extends ActivityEditor<T>
{
	public static abstract class TimePickerFragment extends DialogFragment implements
			TimePickerDialog.OnTimeSetListener
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

			// final int year = c.get(Calendar.YEAR);
			// final int month = c.get(Calendar.MONTH);
			// final int day = c.get(Calendar.DAY_OF_MONTH);
			// datePicker.updateDate(year, month, day);

			int hour = c.get(Calendar.HOUR_OF_DAY);
			int minute = c.get(Calendar.MINUTE);

			// Create a new instance of TimePickerDialog and return it
			return new TimePickerDialog(getActivity(), this, hour, minute, DateFormat.is24HourFormat(getActivity()));
		}
	}

	/* =========================== PROTECTED METHODS ================================ */

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
				onTimeChanged(c.getTime());
			}
		};
		// Bundle bundle = new Bundle();
		// bundle.putLong(KEY_TIME, entity.getData().getTime().getTime());
		// newFragment.setArguments(bundle);
		newFragment.show(getFragmentManager(), "timePicker");
	}

	protected abstract void onTimeChanged(Date time);
}
