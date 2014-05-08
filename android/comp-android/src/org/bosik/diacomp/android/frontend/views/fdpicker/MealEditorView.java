package org.bosik.diacomp.android.frontend.views.fdpicker;

import org.bosik.diacomp.android.R;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.LinearLayout;

public class MealEditorView extends LinearLayout
{
	public MealEditorView(Context context)
	{
		super(context);
		init(context);
	}

	public MealEditorView(Context context, AttributeSet attrs)
	{
		super(context, attrs);
		init(context);
	}

	public MealEditorView(Context context, AttributeSet attrs, int defStyle)
	{
		super(context, attrs, defStyle);
		init(context);
	}

	private void init(Context context)
	{
		LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.view_mealeditorview, this);

	}
}
