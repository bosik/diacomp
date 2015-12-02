/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package org.bosik.diacomp.android.frontend.views;

import android.content.Context;
import android.util.AttributeSet;
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

				height += itemHeight;
			}

			return height + getDividerHeight() * (getCount() - 1);
		}
	}
}