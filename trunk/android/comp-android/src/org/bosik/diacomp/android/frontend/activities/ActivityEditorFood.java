package org.bosik.diacomp.android.frontend.activities;

import org.bosik.diacomp.android.R;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;

public class ActivityEditorFood extends ActivityEditor<Versioned<FoodItem>>
{
	// private static final String TAG = ActivityEditorFood.class.getSimpleName();

	// components
	private EditText	editValue;
	private Button		buttonOK;

	/* =========================== OVERRIDEN METHODS ================================ */

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.editor_food);
		editValue = (EditText) findViewById(R.id.editFoodName);
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
		if (!createMode)
		{
			editValue.setText(entity.getData().getName());
		}
		else
		{
			editValue.setText("");
		}
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		entity.getData().setName(editValue.getText().toString());
		return true;
	}
}
