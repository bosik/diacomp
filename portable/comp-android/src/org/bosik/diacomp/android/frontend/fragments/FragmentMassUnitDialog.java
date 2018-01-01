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
package org.bosik.diacomp.android.frontend.fragments;

import android.app.AlertDialog;
import android.app.Dialog;
import android.app.DialogFragment;
import android.content.DialogInterface;
import android.os.Bundle;
import org.bosik.diacomp.android.R;

public class FragmentMassUnitDialog extends DialogFragment
{
	public static final String KEY_BU = "field.bu";

	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState)
	{
		int index;
		if (getArguments().containsKey(KEY_BU))
		{
			index = getArguments().getBoolean(KEY_BU) ? 1 : 0;
		}
		else
		{
			index = -1;
		}

		AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
		builder.setTitle(R.string.unit_mass_title);
		builder.setSingleChoiceItems(R.array.unit_mass_options, index, new DialogInterface.OnClickListener()
		{
			@Override
			public void onClick(DialogInterface dialog, int which)
			{
				dialog.dismiss();
				((DialogInterface.OnClickListener) getActivity()).onClick(dialog, which);
			}
		});
		return builder.create();
	}
}
