package org.bosik.diacomp.android.frontend.views;

import java.util.HashMap;
import android.content.Context;
import android.util.AttributeSet;
import android.widget.AutoCompleteTextView;

public class FoodDishTextView extends AutoCompleteTextView
{
	public FoodDishTextView(Context context, AttributeSet attrs)
	{
		super(context, attrs);
	}

	@Override
	protected CharSequence convertSelectionToString(Object selectedItem)
	{
		@SuppressWarnings("unchecked")
		HashMap<String, String> hm = (HashMap<String, String>) selectedItem;
		return hm.get("txt");
	}
}