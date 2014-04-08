package org.bosik.diacomp.android.frontend.activities;

import java.util.Date;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.DatePicker;
import android.widget.EditText;
import android.widget.TimePicker;

public class ActivityEditorNote extends ActivityEditor<NoteRecord>
{
	/* =========================== КОНСТАНТЫ ================================ */
	// private static final String TAG = "ActivityEditorNote";

	/* =========================== ПОЛЯ ================================ */

	// компоненты
	private TimePicker	timePicker;
	private DatePicker	datePicker;
	private EditText	editText;
	private Button		buttonOK;

	// TODO: localize error message
	private static final String	ERROR_INCORRECT_NOTE_VALUE	= "Ошибка: неверный текст";

	/* =========================== МЕТОДЫ ================================ */

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.editor_note);
		timePicker = (TimePicker) findViewById(R.id.pickerNoteTime);
		timePicker.setIs24HourView(true);
		datePicker = (DatePicker) findViewById(R.id.pickerNoteDate);
		editText = (EditText) findViewById(R.id.editNoteText);
		buttonOK = (Button) findViewById(R.id.buttonNoteOK);
		buttonOK.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				ActivityEditorNote.this.submit();
			}
		});
		timePicker.setIs24HourView(true);
	}

	@Override
	protected void showValuesInGUI(boolean createMode)
	{
		if (!createMode)
		{
			showTime(entity.getData().getTime(), datePicker, timePicker);
			editText.setText(entity.getData().getText());
		}
		else
		{
			showTime(new Date(), datePicker, timePicker);
			editText.setText("");
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
			UIUtils.showTip(ActivityEditorNote.this, ERROR_INCORRECT_TIME);
			timePicker.requestFocus();
			return false;
		}

		// text
		try
		{
			entity.getData().setText(editText.getText().toString());
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(ActivityEditorNote.this, ERROR_INCORRECT_NOTE_VALUE);
			editText.requestFocus();
			return false;
		}

		return true;
	}
}
