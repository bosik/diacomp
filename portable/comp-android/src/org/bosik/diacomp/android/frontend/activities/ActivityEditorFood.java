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

import android.view.View;
import android.view.View.OnClickListener;
import android.widget.EditText;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.utils.Utils;

public class ActivityEditorFood extends ActivityEditor<FoodItem>
{
	// ui
	private EditText editName;
	private EditText editProts;
	private EditText editFats;
	private EditText editCarbs;
	private EditText editValue;

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.activity_editor_food);
		editName = (EditText) findViewById(R.id.editFoodName);
		editProts = (EditText) findViewById(R.id.editFoodProts);
		editFats = (EditText) findViewById(R.id.editFoodFats);
		editCarbs = (EditText) findViewById(R.id.editFoodCarbs);
		editValue = (EditText) findViewById(R.id.editFoodValue);

		findViewById(R.id.buttonFoodOK).setOnClickListener(new OnClickListener()
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
			editProts.setText(Utils.formatDoubleShort(entity.getData().getRelProts()));
			editFats.setText(Utils.formatDoubleShort(entity.getData().getRelFats()));
			editCarbs.setText(Utils.formatDoubleShort(entity.getData().getRelCarbs()));
			editValue.setText(Utils.formatDoubleShort(entity.getData().getRelValue()));
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

	private void readDouble(EditText editor, Setter setter)
	{
		try
		{
			setter.set(Utils.parseExpression(editor.getText().toString()));
		}
		catch (IllegalArgumentException e)
		{
			editor.requestFocus();
			UIUtils.showTip(this, getString(R.string.common_tip_error_invalid_decimal));
			throw e;
		}
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		String name = editName.getText().toString();
		if (name == null)
		{
			UIUtils.showTip(this, getString(R.string.editor_food_error_empty_name));
			editName.requestFocus();
			return false;
		}

		name = name.trim();
		String nameCleared = Utils.removeNonUtf8(name).trim();

		if (!name.equals(nameCleared))
		{
			editName.setText(nameCleared);
			UIUtils.showTip(this, getString(R.string.common_tip_unsupported_chars_removed));
		}

		if (nameCleared.isEmpty())
		{
			UIUtils.showTip(this, getString(R.string.editor_food_error_empty_name));
			editName.requestFocus();
			return false;
		}

		entity.getData().setName(nameCleared);

		// =============================================================

		try
		{
			readDouble(editProts, new Setter()
			{
				@Override
				public void set(double value)
				{
					entity.getData().setRelProts(value);
				}
			});
			readDouble(editFats, new Setter()
			{
				@Override
				public void set(double value)
				{
					entity.getData().setRelFats(value);
				}
			});
			readDouble(editCarbs, new Setter()
			{
				@Override
				public void set(double value)
				{
					entity.getData().setRelCarbs(value);
				}
			});
			readDouble(editValue, new Setter()
			{
				@Override
				public void set(double value)
				{
					entity.getData().setRelValue(value);
				}
			});
			return true;
		}
		catch (IllegalArgumentException e)
		{
			return false;
		}
	}
}
