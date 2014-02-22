package org.bosik.diacomp.android.face.activities;

import java.util.Date;
import org.bosik.diacomp.android.face.UIUtils;
import org.bosik.diacomp.core.bo.diary.records.BloodRecord;
import org.bosik.diacomp.core.persistence.common.Versioned;
import org.bosik.diacomp.android.face.R;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.DatePicker;
import android.widget.EditText;
import android.widget.Spinner;
import android.widget.TextView;
import android.widget.TimePicker;

public class ActivityEditorBlood extends ActivityEditor<Versioned<BloodRecord>>
{
	// private static final String TAG = ActivityEditorBlood.class.getSimpleName();

	// components
	private TimePicker	timePicker;
	private DatePicker	datePicker;
	private EditText	editValue;
	private TextView	labelBloodFinger;
	private Spinner		spinnerFinger;
	private Button		buttonOK;

	// parameters
	private boolean		askFinger	= true;

	/* =========================== OVERRIDEN METHODS ================================ */

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.editor_blood);
		timePicker = (TimePicker) findViewById(R.id.pickerBloodTime);
		timePicker.setIs24HourView(true);
		datePicker = (DatePicker) findViewById(R.id.pickerBloodDate);
		editValue = (EditText) findViewById(R.id.editBloodValue);
		labelBloodFinger = (TextView) findViewById(R.id.labelBloodFinger);
		spinnerFinger = (Spinner) findViewById(R.id.spinnerBloodFinger);
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
		spinnerFinger.setSelection(entity.getData().getFinger());

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

		// time
		try
		{
			entity.getData().setTime(readTime(datePicker, timePicker));
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
			UIUtils.showTip(this, "Введите корректное значение СК");
			editValue.requestFocus();
			return false;
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(this, "Введите корректное значение СК");
			editValue.requestFocus();
			return false;
		}

		// finger
		try
		{
			if (askFinger)
			{
				entity.getData().setFinger(spinnerFinger.getSelectedItemPosition());
			}
			else
			{
				entity.getData().setFinger(-1);
			}
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(this, "Укажите палец, из которого бралась кровь");
			spinnerFinger.requestFocus();
			return false;
		}

		return true;
	}
}
