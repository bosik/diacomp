package org.bosik.diacomp.android.frontend.views.fdpicker;

import java.util.HashMap;
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
	public static String	FIELD_ICON		= "itemIcon";
	public static String	FIELD_CAPTION	= "itemCaption";

	public FoodDishTextView(Context context, AttributeSet attrs)
	{
		super(context, attrs);
	}

	@Override
	protected CharSequence convertSelectionToString(Object selectedItem)
	{
		@SuppressWarnings("unchecked")
		HashMap<String, String> hm = (HashMap<String, String>) selectedItem;
		return hm.get(FIELD_CAPTION);
	}
}