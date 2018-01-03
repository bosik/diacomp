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

import android.app.TimePickerDialog;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.TimePicker;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.frontend.fragments.pickers.TimePickerFragment;
import org.bosik.diacomp.core.entities.business.Rate;
import org.bosik.diacomp.core.utils.Utils;

import java.util.Arrays;
import java.util.Calendar;
import java.util.LinkedHashSet;

public class ActivityEditorRate extends ActivityEditor<Rate> implements TimePickerDialog.OnTimeSetListener
{
	public static final String KEY_INTENT_USE_BU = "org.bosik.diacomp.useBU";

	private static final int INDEX_K = 1;
	private static final int INDEX_Q = 2;
	private static final int INDEX_X = 3;

	// components
	private Button   buttonTime;
	private EditText editK;
	private EditText editQ;
	private EditText editX;

	private boolean BU;
	private boolean ignoreUpdates = false;

	/* =========================== OVERRIDDEN METHODS ================================ */

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.activity_editor_rate);

		BU = getIntent().getExtras().getBoolean(KEY_INTENT_USE_BU, false);

		buttonTime = (Button) findViewById(R.id.buttonRateTime);
		buttonTime.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				TimePickerFragment newFragment = new TimePickerFragment();
				Bundle args = new Bundle();

				Calendar c = Calendar.getInstance();
				c.set(Calendar.HOUR_OF_DAY, entity.getData().getTime() / Utils.MinPerHour);
				c.set(Calendar.MINUTE, entity.getData().getTime() % Utils.MinPerHour);
				c.set(Calendar.SECOND, 0);
				c.set(Calendar.MILLISECOND, 0);

				args.putLong(TimePickerFragment.KEY, c.getTimeInMillis());
				newFragment.setArguments(args);
				newFragment.show(getFragmentManager(), "timePicker");
			}
		});

		((TextView) findViewById(R.id.labelRateK)).setText(
				String.format("%s, %s/%s", getString(R.string.common_koof_k), getString(R.string.common_unit_bs_mmoll),
						BU ? getString(R.string.common_unit_mass_bu) : getString(R.string.common_unit_mass_gramm)));
		((TextView) findViewById(R.id.labelRateQ)).setText(
				String.format("%s, %s/%s", getString(R.string.common_koof_q), getString(R.string.common_unit_bs_mmoll),
						getString(R.string.common_unit_insulin)));
		((TextView) findViewById(R.id.labelRateX)).setText(
				String.format("%s, %s/%s", getString(R.string.common_koof_x), getString(R.string.common_unit_insulin),
						BU ? getString(R.string.common_unit_mass_bu) : getString(R.string.common_unit_mass_gramm)));

		editK = (EditText) findViewById(R.id.editRateK);
		editQ = (EditText) findViewById(R.id.editRateQ);
		editX = (EditText) findViewById(R.id.editRateX);

		LinkedHashSet<Integer> indexes = new LinkedHashSet<>(Arrays.asList(INDEX_X, INDEX_Q, INDEX_K));
		editK.addTextChangedListener(new MyTextWatcher(indexes, INDEX_K));
		editQ.addTextChangedListener(new MyTextWatcher(indexes, INDEX_Q));
		editX.addTextChangedListener(new MyTextWatcher(indexes, INDEX_X));

		findViewById(R.id.buttonHelpK).setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				UIUtils.showLongTip(ActivityEditorRate.this, getString(R.string.common_rate_k_description));
			}
		});
		findViewById(R.id.buttonHelpQ).setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				UIUtils.showLongTip(ActivityEditorRate.this, getString(R.string.common_rate_q_description));
			}
		});
		findViewById(R.id.buttonHelpX).setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				UIUtils.showLongTip(ActivityEditorRate.this, getString(R.string.common_rate_x_description));
			}
		});

		findViewById(R.id.buttonRateOK).setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				ActivityEditorRate.this.submit();
			}
		});
	}

	@Override
	protected void showValuesInGUI(boolean createMode)
	{
		buttonTime.setText(Utils.formatTimeMin(entity.getData().getTime()));

		if (!createMode)
		{
			// sic: reversed
			editX.setText(Utils.formatX(entity.getData().getK() / entity.getData().getQ(), BU));
			editQ.setText(Utils.formatQ(entity.getData().getQ()));
			editK.setText(Utils.formatK(entity.getData().getK(), BU));
		}
		else
		{
			editK.setText("");
			editQ.setText("");
			editX.setText("");
		}
	}

	private boolean readDouble(EditText editor)
	{
		boolean correct;
		try
		{
			correct = (Utils.parseExpression(editor.getText().toString()) > 0);
		}
		catch (IllegalArgumentException e)
		{
			correct = false;
		}

		if (!correct)
		{
			UIUtils.showTip(this, getString(R.string.editor_rate_error_invalid_value));
			editor.requestFocus();
		}

		return correct;
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		return readDouble(editK) && readDouble(editQ) && readDouble(editX);
	}

	@Override
	public void onTimeSet(TimePicker view, int hourOfDay, int minute)
	{
		entity.getData().setTime(hourOfDay * Utils.MinPerHour + minute);
		buttonTime.setText(Utils.formatTimeMin(entity.getData().getTime()));
	}

	private class MyTextWatcher implements TextWatcher
	{
		private final LinkedHashSet<Integer> indexes;
		private final int                    index;

		MyTextWatcher(LinkedHashSet<Integer> indexes, int index)
		{
			this.indexes = indexes;
			this.index = index;
		}

		@Override
		public void beforeTextChanged(CharSequence s, int start, int count, int after)
		{
		}

		@Override
		public void onTextChanged(CharSequence s, int start, int before, int count)
		{
		}

		@Override
		public void afterTextChanged(Editable s)
		{
			if (!ignoreUpdates)
			{
				ignoreUpdates = true;
				try
				{
					double value = Double.parseDouble(s.toString());

					if (value > 0)
					{
						double k = entity.getData().getK();
						double q = entity.getData().getQ();
						double x = entity.getData().getK() / entity.getData().getQ();

						switch (index)
						{
							case INDEX_K:
							{
								k = (BU ? value / Utils.CARB_PER_BU : value);
								editK.setTextColor(getResources().getColor(R.color.font_black));
								break;
							}
							case INDEX_Q:
							{
								q = value;
								editQ.setTextColor(getResources().getColor(R.color.font_black));
								break;
							}
							case INDEX_X:
							{
								x = (BU ? value / Utils.CARB_PER_BU : value);
								editX.setTextColor(getResources().getColor(R.color.font_black));
								break;
							}
						}

						moveToEnd(indexes, index);
						switch (indexes.iterator().next())
						{
							case INDEX_K:
							{
								k = x * q;
								editK.setText(Utils.formatK(k, BU));
								editK.setTextColor(getResources().getColor(R.color.font_gray));
								break;
							}
							case INDEX_Q:
							{
								q = k / x;
								editQ.setText(Utils.formatQ(q));
								editQ.setTextColor(getResources().getColor(R.color.font_gray));
								break;
							}
							case INDEX_X:
							{
								x = k / q;
								editX.setText(Utils.formatX(x, BU));
								editX.setTextColor(getResources().getColor(R.color.font_gray));
								break;
							}
						}

						entity.getData().setK(k);
						entity.getData().setQ(q);
					}
				}
				catch (NumberFormatException e)
				{
					// ignore
				}
				finally
				{
					ignoreUpdates = false;
				}
			}
		}

		private void moveToEnd(LinkedHashSet<Integer> indexes, int value)
		{
			indexes.remove(value);
			indexes.add(value);
		}
	}
}
