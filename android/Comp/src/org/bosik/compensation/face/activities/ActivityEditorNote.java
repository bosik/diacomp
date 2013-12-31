package org.bosik.compensation.face.activities;

import java.util.Date;
import org.bosik.compensation.bo.diary.records.NoteRecord;
import org.bosik.compensation.face.R;
import org.bosik.compensation.face.UIUtils;
import org.bosik.compensation.persistence.common.Versioned;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TimePicker;

public class ActivityEditorNote extends ActivityEditor<Versioned<NoteRecord>>
{
	/* =========================== КОНСТАНТЫ ================================ */
	// private static final String TAG = "ActivityEditorNote";

	/* =========================== ПОЛЯ ================================ */

	// компоненты
	private TimePicker	timePicker;
	private EditText	editText;
	private Button		buttonOK;

	/* =========================== МЕТОДЫ ================================ */

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.editor_note);
		editText = (EditText) findViewById(R.id.editNoteText);
		timePicker = (TimePicker) findViewById(R.id.pickerNoteTime);
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
		// FIXME: add date picker
		final Date time = entity.getData().getTime();
		// datePicker.updateDate(time.getYear(), time.getMonth(), time.getDate());
		timePicker.setCurrentHour(time.getHours());
		timePicker.setCurrentMinute(time.getMinutes());

		if (!createMode)
		{
			editText.setText(entity.getData().getText());
		}
		else
		{
			editText.setText("");
		}
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		// читаем время
		try
		{
			final int year = 2013;// datePicker.getYear();
			final int month = 12;// datePicker.getMonth();
			final int day = 29;// datePicker.getDayOfMonth();
			final Integer hour = timePicker.getCurrentHour();
			final Integer minute = timePicker.getCurrentMinute();
			Date time = new Date(year, month, day, hour, minute);
			entity.getData().setTime(time);
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(ActivityEditorNote.this, "Ошибка: неверное время");
			timePicker.requestFocus();
			return false;
		}

		// читаем значение
		try
		{
			entity.getData().setText(editText.getText().toString());
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(ActivityEditorNote.this, "Ошибка: неверный текст");
			editText.requestFocus();
			return false;
		}

		return true;
	}

}
