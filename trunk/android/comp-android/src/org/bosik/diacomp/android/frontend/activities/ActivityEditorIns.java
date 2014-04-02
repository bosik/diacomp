package org.bosik.diacomp.android.frontend.activities;

import java.util.Date;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.tech.Versioned;
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

	// TODO: localize error messages
	final String		ERROR_INCORECT_INS_VALUE	= "Введите корректное значение инъекции";

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
		if (!createMode)
		{
			showTime(entity.getData().getTime(), datePicker, timePicker);
			editValue.setText(String.valueOf(entity.getData().getValue()));
		}
		else
		{
			showTime(new Date(), datePicker, timePicker);
			editValue.setText("");
		}
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		// time
		try
		{
			entity.getData().setTime(readTime(datePicker, timePicker));
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(this, ERROR_INCORRECT_TIME);
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
			UIUtils.showTip(this, ERROR_INCORECT_INS_VALUE);
			editValue.requestFocus();
			return false;
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(this, ERROR_INCORECT_INS_VALUE);
			editValue.requestFocus();
			return false;
		}

		return true;
	}
}
