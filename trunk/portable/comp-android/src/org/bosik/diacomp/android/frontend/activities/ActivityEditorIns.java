package org.bosik.diacomp.android.frontend.activities;

import java.util.Date;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;

public class ActivityEditorIns extends ActivityEditorTime<InsRecord>
{
	// components
	private Button		buttonTime;
	private Button		buttonDate;
	private EditText	editValue;
	private Button		buttonOK;

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.activity_editor_ins);
		buttonTime = (Button) findViewById(R.id.buttonInsTime);
		buttonTime.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				showTimePickerDialog();
			}
		});
		buttonDate = (Button) findViewById(R.id.buttonInsDate);
		buttonDate.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				showDatePickerDialog();
			}
		});
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
			onDateTimeChanged(entity.getData().getTime());
			editValue.setText(String.valueOf(entity.getData().getValue()));
		}
		else
		{
			onDateTimeChanged(new Date());
			editValue.setText("");
		}
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		final String ERROR_INCORECT_INS_VALUE = getString(R.string.editor_ins_error_invalid_dosage);

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

	@Override
	protected void onDateTimeChanged(Date time)
	{
		buttonTime.setText(formatTime(time));
		buttonDate.setText(formatDate(time));
	}
}
