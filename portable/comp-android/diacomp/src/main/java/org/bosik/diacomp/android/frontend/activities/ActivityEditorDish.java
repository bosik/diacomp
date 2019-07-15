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

import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.EditText;
import android.widget.ToggleButton;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.frontend.UIUtils.OnSubmit;
import org.bosik.diacomp.android.frontend.views.fdpicker.MealEditorView;
import org.bosik.diacomp.android.frontend.views.fdpicker.MealEditorView.OnChangeListener;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.utils.Utils;

import java.util.ArrayList;
import java.util.List;

public class ActivityEditorDish extends ActivityEditor<DishItem>
{
	private static final String KEY_MODIFIED = "org.bosik.diacomp.editor.dish.modified";

	// data
	private boolean modified;

	// components
	private EditText       editName;
	private ToggleButton   buttonMass;
	private MealEditorView editor;

	// ======================================================================================================

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		if (savedInstanceState != null)
		{
			if (savedInstanceState.containsKey(KEY_MODIFIED))
			{
				modified = savedInstanceState.getBoolean(KEY_MODIFIED);
			}
		}
	}

	@Override
	protected void onSaveInstanceState(Bundle outState)
	{
		super.onSaveInstanceState(outState);

		if (outState != null)
		{
			outState.putBoolean(KEY_MODIFIED, modified);
		}
	}

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.activity_editor_dish);

		// components
		editName = findViewById(R.id.editDishName);
		editName.addTextChangedListener(new TextWatcher()
		{
			@Override
			public void afterTextChanged(Editable s)
			{
				if (!s.toString().equals(entity.getData().getName()))
				{
					modified = true;
				}
			}

			@Override
			public void beforeTextChanged(CharSequence s, int start, int count, int after)
			{
			}

			@Override
			public void onTextChanged(CharSequence s, int start, int before, int count)
			{
			}
		});

		editName.setMaxLines(Integer.MAX_VALUE);
		editName.setHorizontallyScrolling(false);

		editor = findViewById(R.id.dishEditor);
		editor.setOnChangeListener(new OnChangeListener()
		{
			@Override
			public void onChange(List<FoodMassed> items)
			{
				modified = true;

				entity.getData().clear();
				for (FoodMassed item : editor.getData())
				{
					entity.getData().add(item);
				}
			}
		});
		buttonMass = findViewById(R.id.buttonDishMass);
		buttonMass.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				showMass();

				final String title = getString(R.string.editor_dish_rm_title);
				final String message = getString(R.string.editor_dish_rm_hint);
				final String defaultMass = "";

				UIUtils.requestMass(ActivityEditorDish.this, title, message, defaultMass, new OnSubmit()
				{
					@Override
					public void onSubmit(Double mass)
					{
						if (mass == null)
						{
							entity.getData().setMass(null);
						}
						else
						{
							entity.getData().setMass(mass);
						}
						showMass();
						modified = true;
					}

					@Override
					public void onCancel()
					{
						showMass();
					}
				});
			}
		});
	}

	private void showMassOn(double mass)
	{
		buttonMass.setChecked(true);
		buttonMass.setText(Utils.formatDoubleShort(mass) + " " + getString(R.string.common_unit_mass_gramm));
	}

	private void showMassOff()
	{
		buttonMass.setChecked(false);
		buttonMass.setText(getString(R.string.editor_dish_rm_empty));
	}

	private void showMass()
	{
		if (entity.getData().getMass() == null)
		{
			showMassOff();
		}
		else
		{
			showMassOn(entity.getData().getMass());
		}
	}

	private void showDish()
	{
		List<FoodMassed> items = new ArrayList<>();

		for (int i = 0; i < entity.getData().count(); i++)
		{
			items.add(entity.getData().get(i));
		}

		editor.setData(items);
	}

	@Override
	public void onBackPressed()
	{
		// THINK: what is proper behavior here? Do I need Save button?
		if (modified)
		{
			submit();
		}
		else
		{
			super.onBackPressed();
		}
	}

	@Override
	protected void showValuesInGUI(boolean createMode)
	{
		showDish();
		editName.setText(entity.getData().getName());
		showMass();
		modified = false;
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		String name = editName.getText().toString();
		if (name == null)
		{
			UIUtils.showTip(this, getString(R.string.editor_dish_error_empty_name));
			editName.requestFocus();
			return false;
		}

		name = name.trim();
		String nameCleared = Utils.removeNonUtf8(name).trim();

		if (!name.equals(nameCleared))
		{
			UIUtils.showTip(this, getString(R.string.common_tip_unsupported_chars_removed));
			editName.setText(nameCleared);
		}

		if (nameCleared.isEmpty())
		{
			editName.setText(nameCleared);
			editName.requestFocus();
			UIUtils.showTip(this, getString(R.string.editor_dish_error_empty_name));
			return false;
		}

		entity.getData().setName(nameCleared);

		// =============================================================

		entity.getData().clear();

		for (FoodMassed item : editor.getData())
		{
			entity.getData().add(item);
		}

		return true;
	}
}