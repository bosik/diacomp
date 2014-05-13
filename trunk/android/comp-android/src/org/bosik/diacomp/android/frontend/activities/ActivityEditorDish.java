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
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ToggleButton;

public class ActivityEditorDish extends ActivityEditor<DishItem>
{
	// TODO: localize error messages
	private static final String	MSG_INCORRECT_VALUE	= "Введите корректное значение";
	private static final String	MSG_EMPTY_NAME		= "Введите название";

	// data
	boolean						modified;

	// components
	private EditText			editName;
	private MealEditorView		editor;
	ToggleButton				buttonMass;
	private Button				buttonOK;

	// localization
	private String				captionGramm;

	// ======================================================================================================

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.activity_editor_dish);

		// string constants
		captionGramm = getString(R.string.common_gramm);

		// components
		editName = (EditText) findViewById(R.id.editDishName);
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
				// TODO: localize
				final String title = "Change dish mass";
				final String message = "Dish mass:";
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

		modified = false;
	}

	void showMassOn(double mass)
	{
		buttonMass.setChecked(true);
		buttonMass.setText(Utils.formatDoubleShort(mass) + " " + captionGramm);
	}

	void showMassOff()
	{
		buttonMass.setChecked(false);
		// TODO: localize
		buttonMass.setText("No mass");
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

		if (!createMode)
		{
			editName.setText(entity.getData().getName());
		}
		else
		{
			editName.setText("");
		}

		showMass();
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

		entity.getData().clear();

		for (FoodMassed item : editor.getData())
		{
			entity.getData().add(item);
		}

		return true;
	}
}