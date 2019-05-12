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

import android.os.Parcel;
import android.os.Parcelable;
import android.preference.Preference.BaseSavedState;

public class BooleanState extends BaseSavedState
{
	private boolean value;

	public BooleanState(Parcelable state)
	{
		super(state);
	}

	public BooleanState(Parcel in)
	{
		super(in);
		value = (in.readByte() == 1);
	}

	@Override
	public int describeContents()
	{
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags)
	{
		super.writeToParcel(dest, flags);
		dest.writeByte((byte) (value ? 1 : 0));
	}

	public static final Parcelable.Creator<BooleanState> CREATOR = new Parcelable.Creator<BooleanState>()
	{
		public BooleanState createFromParcel(Parcel in)
		{
			return new BooleanState(in);
		}

		public BooleanState[] newArray(int size)
		{
			return new BooleanState[size];
		}
	};

	public boolean getValue()
	{
		return value;
	}

	public void setValue(boolean value)
	{
		this.value = value;
	}
}