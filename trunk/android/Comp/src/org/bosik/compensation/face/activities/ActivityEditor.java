package org.bosik.compensation.face.activities;

import java.io.Serializable;
import org.bosik.compensation.bo.diary.records.DiaryRecord;
import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;

public abstract class ActivityEditor<T extends Serializable> extends Activity
{
	private static final String	TAG					= ActivityEditor.class.getSimpleName();

	public static final String	FIELD_CREATEMODE	= "bosik.pack.createMode";
	public static final String	FIELD_ENTITY		= "bosik.pack.entity";

	protected T					entity;

	/* =========================== АБСТРАКТНЫЕ МЕТОДЫ ================================ */

	/**
	 * Get data from Intent
	 * 
	 * @param intent
	 */
	@SuppressWarnings("unchecked")
	private void readValues(Intent intent)
	{
		entity = (T) intent.getExtras().getSerializable(FIELD_ENTITY);
	}

	/**
	 * Write data to Intent
	 * 
	 * @param intent
	 */
	private void writeValues(Intent intent)
	{
		intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
	}

	/**
	 * Show data in GUI
	 */
	protected abstract void showValuesInGUI(boolean createMode);

	/**
	 * Read and validate data from GUI
	 * 
	 * @return
	 */
	protected abstract boolean getValuesFromGUI();

	/**
	 * Поиск всех необходимых компонентов. В нём необходимо реализовать:<br/>
	 * * установку макета (setContentView())<br/>
	 * * определение всех необходимых компонентов<br/>
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
		// Utils.logTimer(TAG, "setClickListener");

		// получаем данные и выводим их в компоненты
		readValues(getIntent());
		// Utils.logTimer(TAG, "readValues");

		boolean createMode = getIntent().getBooleanExtra(FIELD_CREATEMODE, true);
		showValuesInGUI(createMode);
		// Utils.logTimer(TAG, "setValues");
	}

	protected void submit()
	{
		if (getValuesFromGUI())
		{
			Intent intent = getIntent();
			writeValues(intent);
			setResult(RESULT_OK, intent);
			finish();
		}
	}
}
