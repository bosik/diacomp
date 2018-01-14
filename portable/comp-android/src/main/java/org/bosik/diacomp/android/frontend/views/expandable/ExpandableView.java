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
package org.bosik.diacomp.android.frontend.views.expandable;

import org.bosik.diacomp.android.R;
import android.content.Context;
import android.graphics.drawable.Drawable;
import android.os.Parcelable;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;
import android.widget.LinearLayout;

public class ExpandableView extends LinearLayout
{
	private Button				groupSwitch;
	private View				contentPanel;
	private OnSwitchedListener	onSwitchedListener;
	private boolean				expanded;

	@Override
	protected Parcelable onSaveInstanceState()
	{
		Parcelable superState = super.onSaveInstanceState();
		BooleanState state = new BooleanState(superState);
		state.setValue(isExpanded());
		return state;
	}

	@Override
	protected void onRestoreInstanceState(Parcelable state)
	{
		BooleanState ss = (BooleanState) state;
		super.onRestoreInstanceState(ss.getSuperState());

		setExpanded(ss.getValue());
	}

	public ExpandableView(final Context context, AttributeSet attributes)
	{
		super(context, attributes);

		LayoutInflater inflater = (LayoutInflater) getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.view_expandable, this);

		if (isInEditMode())
		{
			return;
		}

		groupSwitch = (Button) findViewById(R.id.buttonGroupSwitch);
		groupSwitch.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				setExpanded(!isExpanded());
			}
		});
	}

	private View getContentPanel()
	{
		return contentPanel;
	}

	public void setContentPanel(View contentPanel)
	{
		this.contentPanel = contentPanel;
	}

	private OnSwitchedListener getOnSwitchedListener()
	{
		return onSwitchedListener;
	}

	public void setOnSwitchedListener(OnSwitchedListener onSwitchedListener)
	{
		this.onSwitchedListener = onSwitchedListener;
	}

	public CharSequence getTitle()
	{
		return groupSwitch.getText();
	}

	public void setTitle(String title)
	{
		groupSwitch.setText(title);
	}

	public boolean isExpanded()
	{
		return expanded;
	}

	public void setExpanded(boolean expanded)
	{
		if (this.expanded != expanded)
		{
			if (expanded)
			{
				expand();
			}
			else
			{
				collapse();
			}
		}
	}

	private void expand()
	{
		expanded = true;
		Drawable icon = getResources().getDrawable(R.drawable.ic_group_expanded);
		groupSwitch.setCompoundDrawablesWithIntrinsicBounds(icon, null, null, null);

		if (getContentPanel() != null)
		{
			getContentPanel().setVisibility(View.VISIBLE);
		}

		if (getOnSwitchedListener() != null)
		{
			getOnSwitchedListener().onExpanded();
		}
	}

	private void collapse()
	{
		expanded = false;
		Drawable icon = getResources().getDrawable(R.drawable.ic_group_collapsed);
		groupSwitch.setCompoundDrawablesWithIntrinsicBounds(icon, null, null, null);

		if (getContentPanel() != null)
		{
			getContentPanel().setVisibility(View.GONE);
		}

		if (getOnSwitchedListener() != null)
		{
			getOnSwitchedListener().onCollapsed();
		}
	}
}
