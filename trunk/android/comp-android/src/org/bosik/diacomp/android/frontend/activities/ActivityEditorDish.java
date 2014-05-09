package org.bosik.diacomp.android.frontend.activities;

import java.util.ArrayList;
import java.util.List;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.views.fdpicker.MealEditorView;
import org.bosik.diacomp.android.frontend.views.fdpicker.MealEditorView.OnChangeListener;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;

public class ActivityEditorDish extends ActivityEditor<DishItem>
{
	// data
	boolean					modified;

	// components
	private MealEditorView	editor;

	// localization
	private String			captionCarbs;
	private String			captionDose;
	private String			captionGramm;

	// ======================================================================================================

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.activity_editor_dish);

		// string constants
		captionCarbs = getString(R.string.editor_meal_label_carbs);
		captionDose = getString(R.string.editor_meal_label_dose);
		captionGramm = getString(R.string.common_gramm);

		// components
		editor = (MealEditorView) findViewById(R.id.dishEditor);
		editor.setOnChangeListener(new OnChangeListener()
		{
			@Override
			public void onChange(List<FoodMassed> items)
			{
				modified = true;
			}
		});

		modified = false;
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
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		entity.getData().clear();
		
		for (FoodMassed item : editor.getData())
		{
			entity.getData().add(item);
		}

		// TODO: read name
		// TODO: read mass

		return false;
	}
}