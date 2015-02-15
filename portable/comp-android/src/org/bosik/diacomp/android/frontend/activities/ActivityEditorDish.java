package org.bosik.diacomp.android.frontend.activities;

import java.util.ArrayList;
import java.util.List;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.frontend.UIUtils.OnSubmit;
import org.bosik.diacomp.android.frontend.views.fdpicker.MealEditorView;
import org.bosik.diacomp.android.frontend.views.fdpicker.MealEditorView.OnChangeListener;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.utils.Utils;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ToggleButton;

public class ActivityEditorDish extends ActivityEditor<DishItem>
{
	// data
	boolean					modified;

	// components
	private EditText		editName;
	private ToggleButton	buttonMass;
	private MealEditorView	editor;
	// TODO: this button is hidden
	private Button			buttonOK;

	// ======================================================================================================

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.activity_editor_dish);

		// components
		editName = (EditText) findViewById(R.id.editDishName);
		editName.addTextChangedListener(new TextWatcher()
		{
			@Override
			public void afterTextChanged(Editable s)
			{
				modified = true;
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

		editor = (MealEditorView) findViewById(R.id.dishEditor);
		editor.setOnChangeListener(new OnChangeListener()
		{
			@Override
			public void onChange(List<FoodMassed> items)
			{
				modified = true;
			}
		});
		buttonOK = (Button) findViewById(R.id.buttonDishOK);
		buttonOK.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				ActivityEditorDish.this.submit();
			}
		});
		buttonMass = (ToggleButton) findViewById(R.id.buttonDishMass);
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

	void showMassOn(double mass)
	{
		buttonMass.setChecked(true);
		buttonMass.setText(Utils.formatDoubleShort(mass) + " " + getString(R.string.common_unit_mass_gramm));
	}

	void showMassOff()
	{
		buttonMass.setChecked(false);
		buttonMass.setText(getString(R.string.editor_dish_rm_empty));
	}

	void showMass()
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
		List<FoodMassed> items = new ArrayList<FoodMassed>();

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
		final String name = editName.getText().toString();
		if (name == null || name.trim().isEmpty())
		{
			UIUtils.showTip(this, getString(R.string.editor_dish_error_empty_name));
			editName.requestFocus();
			return false;
		}
		entity.getData().setName(name);

		// =============================================================

		entity.getData().clear();

		for (FoodMassed item : editor.getData())
		{
			entity.getData().add(item);
		}

		return true;
	}
}