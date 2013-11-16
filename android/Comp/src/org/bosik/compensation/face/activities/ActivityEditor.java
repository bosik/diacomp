package org.bosik.compensation.face.activities;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

public abstract class ActivityEditor extends Activity implements OnClickListener
{
	@SuppressWarnings("unused")
	private static final String	TAG			= "ActivityEditor";

	/* =========================== ПОЛЯ ================================ */
	protected Button			buttonOK	= null;

	/* =========================== АБСТРАКТНЫЕ МЕТОДЫ ================================ */

	/**
	 * Получение данных из Intent
	 * 
	 * @param intent
	 */
	protected abstract void readValues(Intent intent);

	/**
	 * Запись данных в Intent
	 * 
	 * @param intent
	 */
	protected abstract void writeValues(Intent intent);

	/**
	 * Вывод данных в GUI
	 */
	protected abstract void setValues();

	/**
	 * Получение данных из GUI с валидацией
	 * 
	 * @return
	 */
	protected abstract boolean getValues();

	/**
	 * Поиск всех необходимых компонентов. В нём необходимо реализовать:<br/>
	 * * установку макета (setContentView())<br/>
	 * * определение всех необходимых компонентов<br/>
	 * * определение кнопки buttonOK
	 */
	protected abstract void setupInterface();

	/* =========================== РЕАЛИЗОВАННЫЕ МЕТОДЫ ================================ */

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		// Utils.logTimer(TAG, "Editor started");

		// настраиваем интерфейс
		setupInterface();
		// Utils.logTimer(TAG, "setupInterface()");
		buttonOK.setOnClickListener(this);
		// Utils.logTimer(TAG, "setClickListener");

		// получаем данные и выводим их в компоненты
		readValues(getIntent());
		// Utils.logTimer(TAG, "readValues");
		setValues();
		// Utils.logTimer(TAG, "setValues");
	}

	@Override
	public void onClick(View v)
	{
		if (v.getId() == buttonOK.getId())
		{
			if (getValues())
			{
				Intent intent = getIntent();
				writeValues(intent);
				setResult(RESULT_OK, intent);
				finish();
			}
		}
	}
}
