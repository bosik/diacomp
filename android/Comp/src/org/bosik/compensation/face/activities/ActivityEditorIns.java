package org.bosik.compensation.face.activities;

import org.bosik.compensation.bo.diary.records.InsRecord;
import org.bosik.compensation.face.R;

public class ActivityEditorIns extends ActivityEditor<InsRecord>
{
	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.editor_ins);
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
		return true;
	}
}
