/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package org.bosik.diacomp.android.frontend.activities;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.Versioned;

import java.io.Serializable;

// Do not make it abstract: the android.app.Fragment$InstantiationException may be caused otherwise
@SuppressLint("Registered")
public class ActivityEditor<T extends Serializable> extends Activity
{
	public static final String FIELD_CREATE_MODE = "org.bosik.diacomp.createMode";
	public static final String FIELD_ENTITY      = "org.bosik.diacomp.entity";

	private static final String BUNDLE_ENTITY = "entity";

	/**
	 * Stores the data for editing. Note: as far as passing this data achieved via
	 * serialization/deserialization, the editor's entity is completely unlinked (deeply cloned)
	 * from invoker's one. Thus, feel free to modify it the way you like.
	 */
	protected Versioned<T> entity;

	/* =========================== MAIN METHODS ================================ */

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		if (savedInstanceState != null && savedInstanceState.containsKey(BUNDLE_ENTITY))
		{
			entity = (Versioned<T>) savedInstanceState.get(BUNDLE_ENTITY);
		}
		else
		{
			readEntity(getIntent());
		}

		setupInterface();

		boolean createMode = getIntent().getBooleanExtra(FIELD_CREATE_MODE, true);
		showValuesInGUI(createMode);

		if (createMode)
		{
			entity.setId(HashUtils.generateGuid());
		}
	}

	@Override
	protected void onSaveInstanceState(Bundle outState)
	{
		super.onSaveInstanceState(outState);

		if (outState != null)
		{
			outState.putSerializable(BUNDLE_ENTITY, entity);
		}
	}

	/* =========================== PROTECTED METHODS ================================ */

	/**
	 * Read entity from Intent
	 *
	 * @param intent
	 */
	@SuppressWarnings("unchecked")
	protected void readEntity(Intent intent)
	{
		entity = (Versioned<T>) intent.getExtras().getSerializable(FIELD_ENTITY);
	}

	/**
	 * Write entity to Intent
	 *
	 * @param intent
	 */
	protected void writeEntity(Intent intent)
	{
		intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
	}

	/**
	 * Call this method on "Save" button click
	 */
	protected void submit()
	{
		if (getValuesFromGUI())
		{
			entity.modified();

			Intent intent = getIntent();
			writeEntity(intent);
			setResult(RESULT_OK, intent);
			finish();
		}
	}

	/* =========================== ABSTRACT METHODS ================================ */

	/**
	 * The subject
	 */
	protected void setupInterface()
	{
	}

	/**
	 * Show data in GUI
	 */
	@SuppressWarnings("unused")
	protected void showValuesInGUI(boolean createMode)
	{
	}

	/**
	 * Read and validate data from GUI
	 *
	 * @return True if validation succeed, false otherwise
	 */
	@SuppressWarnings("static-method")
	protected boolean getValuesFromGUI()
	{
		return true;
	}
}
