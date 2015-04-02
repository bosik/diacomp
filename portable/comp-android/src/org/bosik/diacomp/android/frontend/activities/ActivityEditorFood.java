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

import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;

public class ActivityEditorFood extends ActivityEditor<FoodItem>
{
	// private static final String TAG = ActivityEditorFood.class.getSimpleName();

	// TODO: localize error messages
	private static final String	MSG_INCORRECT_VALUE	= "Введите корректное значение";
	private static final String	MSG_EMPTY_NAME		= "Введите название";

	// components
	private EditText			editName;
	private EditText			editProts;
	private EditText			editFats;
	private EditText			editCarbs;
	private EditText			editValue;
	private Button				buttonOK;

	/* =========================== OVERRIDEN METHODS ================================ */

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.activity_editor_food);
		editName = (EditText) findViewById(R.id.editFoodName);
		editProts = (EditText) findViewById(R.id.editFoodProts);
		editFats = (EditText) findViewById(R.id.editFoodFats);
		editCarbs = (EditText) findViewById(R.id.editFoodCarbs);
		editValue = (EditText) findViewById(R.id.editFoodValue);

		buttonOK = (Button) findViewById(R.id.buttonFoodOK);
		buttonOK.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				ActivityEditorFood.this.submit();
			}
		});
	}

	@Override
	protected void showValuesInGUI(boolean createMode)
	{
		editName.setText(entity.getData().getName());

		if (!createMode)
		{
			editProts.setText(String.valueOf(entity.getData().getRelProts()));
			editFats.setText(String.valueOf(entity.getData().getRelFats()));
			editCarbs.setText(String.valueOf(entity.getData().getRelCarbs()));
			editValue.setText(String.valueOf(entity.getData().getRelValue()));
		}
		else
		{
			editProts.setText("");
			editFats.setText("");
			editCarbs.setText("");
			editValue.setText("");
		}
	}

	interface Setter
	{
		void set(double value);
	}

	private boolean readDouble(EditText editor, Setter setter)
	{
		try
		{
			setter.set(Double.parseDouble(editor.getText().toString()));
		}
		catch (NumberFormatException e)
		{
			UIUtils.showTip(this, MSG_INCORRECT_VALUE);
			editor.requestFocus();
			return false;
		}
		catch (IllegalArgumentException e)
		{
			UIUtils.showTip(this, MSG_INCORRECT_VALUE);
			editor.requestFocus();
			return false;
		}

		return true;
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		final String name = editName.getText().toString();
		if (name == null || name.trim().isEmpty())
		{
			UIUtils.showTip(this, MSG_EMPTY_NAME);
			editName.requestFocus();
			return false;
		}
		entity.getData().setName(name);

		// =============================================================

		if (!readDouble(editProts, new Setter()
		{
			@Override
			public void set(double value)
			{
				entity.getData().setRelProts(value);
			}
		}))
		{
			return false;
		}

		// =============================================================

		if (!readDouble(editFats, new Setter()
		{
			@Override
			public void set(double value)
			{
				entity.getData().setRelFats(value);
			}
		}))
		{
			return false;
		}

		// =============================================================

		if (!readDouble(editCarbs, new Setter()
		{
			@Override
			public void set(double value)
			{
				entity.getData().setRelCarbs(value);
			}
		}))
		{
			return false;
		}

		// =============================================================

		if (!readDouble(editValue, new Setter()
		{
			@Override
			public void set(double value)
			{
				entity.getData().setRelValue(value);
			}
		}))
		{
			return false;
		}

		// =============================================================

		return true;
	}
}
