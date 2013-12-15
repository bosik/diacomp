package org.bosik.compensation.face.activities;

import org.bosik.compensation.bo.diary.records.InsRecord;
import org.bosik.compensation.face.R;
import org.bosik.compensation.face.UIUtils;
import org.bosik.compensation.persistence.common.Versioned;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.DatePicker;
import android.widget.EditText;
import android.widget.TimePicker;

public class ActivityEditorIns extends ActivityEditor<Versioned<InsRecord>>
{
	// components
	private TimePicker	timePicker;
	private DatePicker	datePicker;
	private EditText	editValue;
	private Button		buttonOK;

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.editor_ins);
		timePicker = (TimePicker) findViewById(R.id.pickerInsTime);
		timePicker.setIs24HourView(true);
		datePicker = (DatePicker) findViewById(R.id.pickerInsDate);
		editValue = (EditText) findViewById(R.id.editInsValue);
		buttonOK = (Button) findViewById(R.id.buttonInsOK);
		buttonOK.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				ActivityEditorIns.this.submit();
			}
		});
	}

	@Override
	protected void showValuesInGUI(boolean createMode)
	{
		timePicker.setCurrentHour(entity.getData().getTime() / 60);
		timePicker.setCurrentMinute(entity.getData().getTime() % 60);

		if (!createMode)
		{
			editValue.setText(String.valueOf(entity.getData().getValue()));
		}
		else
		{
			editValue.setText("");
		}
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		// TODO: localize error messages

		// time
		try
		{
			entity.getData().setTime((timePicker.getCurrentHour() * 60) + timePicker.getCurrentMinute());
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(this, "Введите корректное время");
			timePicker.requestFocus();
			return false;
		}

		// value
		try
		{
			entity.getData().setValue(Double.parseDouble(editValue.getText().toString()));
		}
		catch (NumberFormatException e)
		{
			UIUtils.showTip(this, "Введите корректное значение инъекции");
			editValue.requestFocus();
			return false;
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(this, "Введите корректное значение инъекции");
			editValue.requestFocus();
			return false;
		}

		return true;
	}
}
