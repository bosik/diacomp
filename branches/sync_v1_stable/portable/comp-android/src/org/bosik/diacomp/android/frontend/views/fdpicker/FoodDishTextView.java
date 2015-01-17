package org.bosik.diacomp.android.frontend.views.fdpicker;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.AutoCompleteTextView;

/**
 * Autocomplete box with icons
 * 
 * @author nkrenev
 * 
 */
public class FoodDishTextView extends AutoCompleteTextView
{
	public FoodDishTextView(Context context, AttributeSet attrs)
	{
		super(context, attrs);
	}

	@Override
	protected CharSequence convertSelectionToString(Object selectedItem)
	{
		return ((Item) selectedItem).getCaption();
	}
}