package org.bosik.compensation.face.activities;

import org.bosik.compensation.bo.diary.records.BloodRecord;
import org.bosik.compensation.face.R;
import org.bosik.compensation.face.UIUtils;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Spinner;
import android.widget.TextView;
import android.widget.TimePicker;

public class ActivityEditorBlood extends ActivityEditor<BloodRecord>
{
	// private static final String TAG = ActivityEditorBlood.class.getSimpleName();

	// components
	private EditText	editValue;
	private TimePicker	timePicker;
	private Spinner		spinnerFinger;
	private Button		buttonOK;
	private TextView	labelBloodFinger;

	// parameters
	private boolean		askFinger	= true;

	// TODO: сделать возможность не спрашивать палец

	/* =========================== OVERRIDEN METHODS ================================ */

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.editor_blood);
		timePicker = (TimePicker) findViewById(R.id.pickerBloodTime);
		timePicker.setIs24HourView(true);
		editValue = (EditText) findViewById(R.id.editBloodValue);
		spinnerFinger = (Spinner) findViewById(R.id.spinnerBloodFinger);
		labelBloodFinger = (TextView) findViewById(R.id.labelBloodFinger);
		buttonOK = (Button) findViewById(R.id.buttonBloodOK);
		buttonOK.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				ActivityEditorBlood.this.submit();
			}
		});
	}

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

		if (!askFinger)
		{
			spinnerFinger.setVisibility(View.GONE);
			labelBloodFinger.setVisibility(View.GONE);
		}
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		// TODO: localize error messages

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
			if (askFinger)
			{
				entity.setFinger(spinnerFinger.getSelectedItemPosition());
			}
			else
			{
				entity.setFinger(-1);
			}
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
