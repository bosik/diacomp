package org.bosik.compensation.face.activities;

import org.bosik.compensation.face.R;
import org.bosik.compensation.face.UIUtils;
import org.bosik.compensation.persistence.entity.diary.records.BloodRecord;
import org.bosik.compensation.persistence.entity.diary.records.DiaryRecord;
import android.content.Intent;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Spinner;
import android.widget.TimePicker;

//TODO: сделать ночной режим (тёмные макеты)

public class ActivityEditorBlood extends ActivityEditor
{
	/* =========================== КОНСТАНТЫ ================================ */
	@SuppressWarnings("unused")
	private static final String	TAG					= "ActivityEditorBlood";

	public static final String	FIELD_TIME			= "bosik.pack.time";
	public static final String	FIELD_VALUE			= "bosik.pack.value";
	public static final String	FIELD_FINGER		= "bosik.pack.finger";
	public static final int		UNDEFINITE_VALUE	= -1;

	/* =========================== ПОЛЯ ================================ */

	// редактируемая запись
	private int					time;
	private double				value;
	private int					finger;

	// компоненты
	private EditText			editValue;
	private TimePicker			timePicker;
	private Spinner				spinnerFinger;

	// private TextView labelBloodFinger; // может использоваться для сокрытия компонента

	/* =========================== МЕТОДЫ ================================ */

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.editor_blood);
		editValue = (EditText) findViewById(R.id.editBloodValue);
		timePicker = (TimePicker) findViewById(R.id.pickerBloodTime);
		spinnerFinger = (Spinner) findViewById(R.id.spinnerBloodFinger);
		buttonOK = (Button) findViewById(R.id.buttonBloodOK);
		// labelBloodFinger = (TextView) findViewById(R.id.labelBloodFinger);

		timePicker.setIs24HourView(true);
	}

	@Override
	protected void readValues(Intent intent)
	{
		time = intent.getIntExtra(FIELD_TIME, 0);
		value = intent.getDoubleExtra(FIELD_VALUE, UNDEFINITE_VALUE);
		finger = intent.getIntExtra(FIELD_FINGER, 0);
	}

	@Override
	protected void writeValues(Intent intent)
	{
		intent.putExtra(FIELD_TIME, time);
		intent.putExtra(FIELD_VALUE, value);
		intent.putExtra(FIELD_FINGER, finger);
	}

	@Override
	protected void setValues()
	{
		timePicker.setCurrentHour(time / 60);
		timePicker.setCurrentMinute(time % 60);
		spinnerFinger.setSelection(finger);

		if (value != UNDEFINITE_VALUE)
		{
			editValue.setText(String.valueOf(value));
		}
		else
		{
			editValue.setText("");
		}

		// TODO: сделать возможность не спрашивать палец
		// spinnerFinger.setVisibility(View.GONE);
		// labelBloodFinger.setVisibility(View.GONE);
	}

	@Override
	protected boolean getValues()
	{
		// читаем время
		time = (timePicker.getCurrentHour() * 60) + timePicker.getCurrentMinute();

		// читаем значение
		try
		{
			value = Double.parseDouble(editValue.getText().toString());
		}
		catch (NumberFormatException e)
		{
			UIUtils.showTip(ActivityEditorBlood.this, "Ошибка: неверное значение СК");
			editValue.requestFocus();
			return false;
		}

		// читаем палец
		finger = spinnerFinger.getSelectedItemPosition();

		// валидируем
		if (!DiaryRecord.checkTime(time))
		{
			UIUtils.showTip(ActivityEditorBlood.this, "Ошибка: неверное время");
			timePicker.requestFocus();
			return false;
		}
		if (!BloodRecord.checkValue(value))
		{
			UIUtils.showTip(ActivityEditorBlood.this, "Ошибка: неверное значение СК");
			editValue.requestFocus();
			return false;
		}
		if (!BloodRecord.checkFinger(finger))
		{
			UIUtils.showTip(ActivityEditorBlood.this, "Ошибка: неверный индекс пальца");
			spinnerFinger.requestFocus();
			return false;
		}

		return true;
	}
}
