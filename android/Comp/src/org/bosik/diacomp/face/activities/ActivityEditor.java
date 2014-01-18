package org.bosik.diacomp.face.activities;

import java.io.Serializable;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.widget.DatePicker;
import android.widget.TimePicker;

public abstract class ActivityEditor<T extends Serializable> extends Activity
{
	// private static final String TAG = ActivityEditor.class.getSimpleName();

	public static final String	FIELD_MODE		= "bosik.pack.createMode";
	public static final String	FIELD_ENTITY	= "bosik.pack.entity";

	protected T					entity;

	/* =========================== UTIL METHODS ================================ */

	/**
	 * Read entity from Intent
	 * 
	 * @param intent
	 */
	@SuppressWarnings("unchecked")
	private void readEntity(Intent intent)
	{
		entity = (T) intent.getExtras().getSerializable(FIELD_ENTITY);
	}

	/**
	 * Write entity to Intent
	 * 
	 * @param intent
	 */
	private void writeEntity(Intent intent)
	{
		intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
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

	/* =========================== ABSTRACT METHODS ================================ */

	/**
	 * The subject
	 */
	protected abstract void setupInterface();

	/**
	 * Show data in GUI
	 */
	protected abstract void showValuesInGUI(boolean createMode);

	/**
	 * Read and validate data from GUI
	 * 
	 * @return True if validation succeed, false otherwise
	 */
	protected abstract boolean getValuesFromGUI();

	/* =========================== MAIN METHODS ================================ */

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setupInterface();
		readEntity(getIntent());
		boolean createMode = getIntent().getBooleanExtra(FIELD_MODE, true);
		showValuesInGUI(createMode);
	}

	protected void submit()
	{
		if (getValuesFromGUI())
		{
			Intent intent = getIntent();
			writeEntity(intent);
			setResult(RESULT_OK, intent);
			finish();
		}
	}
}
