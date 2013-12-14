package org.bosik.compensation.face.activities;

import java.io.Serializable;
import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;

public abstract class ActivityEditor<T extends Serializable> extends Activity
{
	// private static final String TAG = ActivityEditor.class.getSimpleName();

	public static final String	FIELD_MODE		= "bosik.pack.createMode";
	public static final String	FIELD_ENTITY	= "bosik.pack.entity";

	protected T					entity;

	/* =========================== UTIL METHODS ================================ */

	/**
	 * Read entity from Intent
	 * 
	 * @param intent
	 */
	@SuppressWarnings("unchecked")
	private void readEntity(Intent intent)
	{
		entity = (T) intent.getExtras().getSerializable(FIELD_ENTITY);
	}

	/**
	 * Write entity to Intent
	 * 
	 * @param intent
	 */
	private void writeEntity(Intent intent)
	{
		intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
	}

	/* =========================== ABSTRACT METHODS ================================ */

	/**
	 * The subject
	 */
	protected abstract void setupInterface();

	/**
	 * Show data in GUI
	 */
	protected abstract void showValuesInGUI(boolean createMode);

	/**
	 * Read and validate data from GUI
	 * 
	 * @return True if validation succeed, false otherwise
	 */
	protected abstract boolean getValuesFromGUI();

	/* =========================== MAIN METHODS ================================ */

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setupInterface();
		readEntity(getIntent());
		boolean createMode = getIntent().getBooleanExtra(FIELD_MODE, true);
		showValuesInGUI(createMode);
	}

	protected void submit()
	{
		if (getValuesFromGUI())
		{
			Intent intent = getIntent();
			writeEntity(intent);
			setResult(RESULT_OK, intent);
			finish();
		}
	}
}
