package org.bosik.diacomp.android.frontend.views;

import android.content.Context;
import android.graphics.Canvas;
import android.util.AttributeSet;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ListView;

public class ExpandedListView extends ListView
{
	private static final String	TAG			= ExpandedListView.class.getSimpleName();
	private int					mOldCount	= 0;

	public ExpandedListView(Context context, AttributeSet attrs)
	{
		super(context, attrs);
	}

	public View getViewByPosition(int pos)
	{
		final int firstListItemPosition = getFirstVisiblePosition();
		final int lastListItemPosition = firstListItemPosition + getChildCount() - 1;

		if (pos < firstListItemPosition || pos > lastListItemPosition)
		{
			return getAdapter().getView(pos, null, this);
		}
		else
		{
			final int childIndex = pos - firstListItemPosition;
			return getChildAt(childIndex);
		}
	}

	public int getFullHeight()
	{
		if (getCount() == 0)
		{
			return 0;
		}
		else
		{
			int height = 0;

			for (int i = 0; i < getCount(); i++)
			{
				View item = getViewByPosition(i);
				item.measure(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT);
				int itemHeight = item.getMeasuredHeight();

				Log.d(TAG, "Created rec with height " + itemHeight);

				height += itemHeight;
			}

			return height + getDividerHeight() * (getCount() - 1);
		}
	}
}