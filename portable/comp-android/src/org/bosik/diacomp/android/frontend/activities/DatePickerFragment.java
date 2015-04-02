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
package org.bosik.diacomp.android.frontend.activities;

import java.util.Calendar;
import java.util.Date;
import android.app.DatePickerDialog;
import android.app.Dialog;
import android.app.DialogFragment;
import android.os.Bundle;

public abstract class DatePickerFragment extends DialogFragment implements DatePickerDialog.OnDateSetListener
{
	private final Date	time;

	public DatePickerFragment(Date time)
	{
		this.time = time;
	}

	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState)
	{
		Calendar c = Calendar.getInstance();
		c.setTime(time);
		int year = c.get(Calendar.YEAR);
		int month = c.get(Calendar.MONTH);
		int day = c.get(Calendar.DAY_OF_MONTH);

		return new DatePickerDialog(getActivity(), this, year, month, day);
	}
}