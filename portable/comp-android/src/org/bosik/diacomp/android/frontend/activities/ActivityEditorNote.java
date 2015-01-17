package org.bosik.diacomp.android.frontend.activities;

import java.util.Date;
import org.bosik.diacomp.android.v1.R;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;

public class ActivityEditorNote extends ActivityEditorTime<NoteRecord>
{
	/* =========================== КОНСТАНТЫ ================================ */
	// private static final String TAG = "ActivityEditorNote";

	/* =========================== ПОЛЯ ================================ */

	// компоненты
	private Button				buttonTime;
	private Button				buttonDate;
	private EditText			editText;
	private Button				buttonOK;

	// TODO: localize error message
	private static final String	ERROR_INCORRECT_NOTE_VALUE	= "Ошибка: неверный текст";

	/* =========================== МЕТОДЫ ================================ */

	public ActivityEditorNote()
	{
		Log.d("PRFM", "ActivityEditorNote() constructing");
	}

	@Override
	protected void setupInterface()
	{
		Log.d("PRFM", "setupInterface() started");

		setContentView(R.layout.activity_editor_note);

		Log.d("PRFM", "setupInterface() content view setted");

		buttonTime = (Button) findViewById(R.id.buttonNoteTime);
		buttonTime.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				showTimePickerDialog();
			}
		});
		buttonDate = (Button) findViewById(R.id.buttonNoteDate);
		buttonDate.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				showDatePickerDialog();
			}
		});

		Log.d("PRFM", "setupInterface() date/time components are ready");

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

		Log.d("PRFM", "setupInterface() finished");
	}

	@Override
	protected void showValuesInGUI(boolean createMode)
	{
		Log.d("PRFM", "showValuesInGUI() started");
		if (!createMode)
		{
			onDateTimeChanged(entity.getData().getTime());
			editText.setText(entity.getData().getText());
		}
		else
		{
			onDateTimeChanged(new Date());
			editText.setText("");
		}
		Log.d("PRFM", "showValuesInGUI() finished");
	}

	@Override
	protected boolean getValuesFromGUI()
	{
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

	@Override
	protected void onDateTimeChanged(Date time)
	{
		buttonTime.setText(formatTime(time));
		buttonDate.setText(formatDate(time));
	}
}
