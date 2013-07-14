package org.bosik.compensation.face.activities;

import org.bosik.compensation.face.R;
import android.content.Intent;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TimePicker;

public class ActivityEditorNote extends ActivityEditor
{
	/* =========================== КОНСТАНТЫ ================================ */
	@SuppressWarnings("unused")
	private static final String TAG = "ActivityEditorNote";
	
	public static final String FIELD_TIME = "bosik.pack.time";
	public static final String FIELD_TEXT = "bosik.pack.text";
	
	/* =========================== ПОЛЯ ================================ */

	// редактируемая запись
	private int time;
	private String text;

	// компоненты
	private TimePicker timePicker;
	private EditText editText;	

	/* =========================== МЕТОДЫ ================================ */
	
	@Override
	protected void readValues(Intent intent)
	{
		time = intent.getIntExtra(FIELD_TIME, 0);
		text = intent.getStringExtra(FIELD_TEXT);
		
		if (null == text) text = "";
	}

	@Override
	protected void writeValues(Intent intent)
	{
		intent.putExtra(FIELD_TIME, time);
		intent.putExtra(FIELD_TEXT, text);
	}

	@Override
	protected void setValues()
	{
		timePicker.setCurrentHour(time / 60);
		timePicker.setCurrentMinute(time % 60);
		editText.setText(text);
	}

	@Override
	protected boolean getValues()
	{
		// читаем время
		time = timePicker.getCurrentHour() * 60 + timePicker.getCurrentMinute();
		
		// читаем значение
		text = editText.getText().toString();
		
		// всегда всё хорошо :)
		return true;
	}

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.editor_note);
		editText = (EditText) findViewById(R.id.editNoteText);
		timePicker = (TimePicker) findViewById(R.id.pickerNoteTime);
		buttonOK = (Button) findViewById(R.id.buttonNoteOK);
		timePicker.setIs24HourView(true);
	}
}
