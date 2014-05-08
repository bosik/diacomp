package org.bosik.diacomp.android.frontend.activities;

import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.views.fdpicker.MealEditorView;
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
	}

	@Override
	public void onBackPressed()
	{
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
		// TODO Auto-generated method stub

	}

	@Override
	protected boolean getValuesFromGUI()
	{
		// TODO Auto-generated method stub
		return false;
	}
}