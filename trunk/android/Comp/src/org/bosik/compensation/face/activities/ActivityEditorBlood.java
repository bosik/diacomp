package org.bosik.compensation.face.activities;

import org.bosik.compensation.bo.diary.records.BloodRecord;
import org.bosik.compensation.face.R;
import org.bosik.compensation.face.UIUtils;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Spinner;
import android.widget.TimePicker;

//TODO: сделать ночной режим (тёмные макеты)

public class ActivityEditorBlood extends ActivityEditor<BloodRecord>
{
	/* =========================== КОНСТАНТЫ ================================ */
	//private static final String	TAG	= ActivityEditorBlood.class.getSimpleName();

	/* =========================== ПОЛЯ ================================ */

	// компоненты
	private EditText			editValue;
	private TimePicker			timePicker;
	private Spinner				spinnerFinger;
	private Button				buttonOK;

	// private TextView labelBloodFinger; // может использоваться для сокрытия компонента

	/* =========================== МЕТОДЫ ================================ */

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.editor_blood);
		editValue = (EditText) findViewById(R.id.editBloodValue);
		timePicker = (TimePicker) findViewById(R.id.pickerBloodTime);
		spinnerFinger = (Spinner) findViewById(R.id.spinnerBloodFinger);
		// labelBloodFinger = (TextView) findViewById(R.id.labelBloodFinger);
		buttonOK = (Button) findViewById(R.id.buttonBloodOK);
		buttonOK.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				ActivityEditorBlood.this.submit();
			}
		});
		timePicker.setIs24HourView(true);
	}

	// @Override
	// protected void readValues(Intent intent)
	// {
	// entity.setTime(intent.getIntExtra(FIELD_TIME, 0));
	// entity.setValue(intent.getDoubleExtra(FIELD_VALUE, 5.0));
	// entity.setFinger(intent.getIntExtra(FIELD_FINGER, 0));
	// }
	//
	// @Override
	// protected void writeValues(Intent intent)
	// {
	// intent.putExtra(FIELD_TIME, entity.getTime());
	// intent.putExtra(FIELD_VALUE, entity.getValue());
	// intent.putExtra(FIELD_FINGER, entity.getFinger());
	//
	// // intent.putExtra(
	// }

	@Override
	protected void showValuesInGUI(boolean createMode)
	{
		timePicker.setCurrentHour(entity.getTime() / 60);
		timePicker.setCurrentMinute(entity.getTime() % 60);
		spinnerFinger.setSelection(entity.getFinger());

		if (!createMode)
		{
			editValue.setText(String.valueOf(entity.getValue()));
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
	protected boolean getValuesFromGUI()
	{
		// читаем время
		try
		{
			entity.setTime((timePicker.getCurrentHour() * 60) + timePicker.getCurrentMinute());
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(ActivityEditorBlood.this, "Ошибка: неверное время");
			timePicker.requestFocus();
			return false;
		}

		// читаем значение
		try
		{
			entity.setValue(Double.parseDouble(editValue.getText().toString()));
		}
		catch (NumberFormatException e)
		{
			UIUtils.showTip(ActivityEditorBlood.this, "Ошибка: неверное значение СК");
			editValue.requestFocus();
			return false;
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(ActivityEditorBlood.this, "Ошибка: неверное значение СК");
			editValue.requestFocus();
			return false;
		}

		// читаем палец
		try
		{
			entity.setFinger(spinnerFinger.getSelectedItemPosition());
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(ActivityEditorBlood.this, "Ошибка: неверный индекс пальца");
			spinnerFinger.requestFocus();
			return false;
		}

		return true;
	}
}
