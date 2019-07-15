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
import org.bosik.diacomp.core.entities.business.TimedRate;
import org.bosik.diacomp.core.entities.business.Units;
import org.bosik.diacomp.core.utils.CodedUtils;
import org.bosik.diacomp.core.utils.Utils;

import java.util.Arrays;
import java.util.Calendar;
import java.util.LinkedHashSet;

public class ActivityEditorRate extends ActivityEditor<TimedRate> implements TimePickerDialog.OnTimeSetListener
{
	public static final String KEY_UNIT_OF_MASS = "org.bosik.diacomp.unitOfMass";

	private static final int INDEX_K = 1;
	private static final int INDEX_Q = 2;
	private static final int INDEX_X = 3;

	// components
	private Button   buttonTime;
	private EditText editK;
	private EditText editQ;
	private EditText editX;

	private boolean ignoreUpdates = false;

	/* =========================== OVERRIDDEN METHODS ================================ */

	@Override
	protected void setupInterface()
	{
		setContentView(R.layout.activity_editor_rate);

		Units.Mass unit = getUnitOfMass();

		buttonTime = findViewById(R.id.buttonRateTime);
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

		String unitTitle = convertUnitToTitle(unit);

		((TextView) findViewById(R.id.labelRateK)).setText(
				String.format("%s, %s/%s", getString(R.string.common_rate_k), getString(R.string.common_unit_bs_mmoll), unitTitle));
		((TextView) findViewById(R.id.labelRateQ)).setText(
				String.format("%s, %s/%s", getString(R.string.common_rate_q), getString(R.string.common_unit_bs_mmoll),
						getString(R.string.common_unit_insulin)));
		((TextView) findViewById(R.id.labelRateX))
				.setText(String.format("%s, %s/%s", getString(R.string.common_rate_x), getString(R.string.common_unit_insulin), unitTitle));

		editK = findViewById(R.id.editRateK);
		editQ = findViewById(R.id.editRateQ);
		editX = findViewById(R.id.editRateX);

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

	private Units.Mass getUnitOfMass()
	{
		String unitCode = getIntent().getExtras().getString(KEY_UNIT_OF_MASS);
		return CodedUtils.parse(Units.Mass.class, unitCode, Utils.DEFAULT_MASS_UNIT);
	}

	private String convertUnitToTitle(Units.Mass unit)
	{
		switch (unit)
		{
			case G:
			{
				return getString(R.string.common_unit_mass_gramm);
			}

			case BU:
			{
				return getString(R.string.common_unit_mass_bu);
			}

			default:
			{
				throw new IllegalArgumentException("Unsupported unit of mass: " + unit);
			}
		}
	}

	@Override
	protected void showValuesInGUI(boolean createMode)
	{
		buttonTime.setText(Utils.formatTimeMin(entity.getData().getTime()));

		if (!createMode)
		{
			Units.Mass unit = getUnitOfMass();

			// sic: reversed
			editX.setText(Utils.formatX(entity.getData().getK() / entity.getData().getQ(), unit));
			editQ.setText(Utils.formatQ(entity.getData().getQ()));
			editK.setText(Utils.formatK(entity.getData().getK(), unit));
		}
		else
		{
			editK.setText("");
			editQ.setText("");
			editX.setText("");
		}
	}

	private void complain(EditText editor)
	{
		UIUtils.showTip(this, getString(R.string.editor_rate_error_invalid_value));
		editor.requestFocus();
	}

	@Override
	protected boolean getValuesFromGUI()
	{
		try
		{
			double valueK = readK();
			double valueQ = readQ();
			double valueX = readX();

			int zeros = countZeros(valueK, valueQ, valueX);

			if (zeros == 0)
			{
				return true;
			}
			else if (zeros == 1)
			{
				if (valueK < Utils.EPS)
				{
					valueK = valueX * valueQ;
				}
				else if (valueQ < Utils.EPS)
				{
					valueQ = valueK / valueX;
				}
				else
				{
					valueX = valueK / valueQ;
				}

				entity.getData().setK(valueK);
				entity.getData().setQ(valueQ);
				entity.getData().setP(0.0);

				return true;
			}
			else
			{
				if (valueK < Utils.EPS)
				{
					complain(editK);
				}
				else if (valueQ < Utils.EPS)
				{
					complain(editQ);
				}
				else
				{
					// this will never happen
					complain(editX);
				}
				return false;
			}
		}
		catch (IllegalArgumentException e)
		{
			return false;
		}
	}

	private double readDouble(EditText editor)
	{
		try
		{
			return Utils.parseExpression(editor.getText().toString());
		}
		catch (IllegalArgumentException e)
		{
			complain(editor);
			throw e;
		}
	}

	private double readK()
	{
		return Units.Mass.convert(readDouble(editK), getUnitOfMass(), Units.Mass.G);
	}

	private double readQ()
	{
		return readDouble(editQ);
	}

	private double readX()
	{
		return Units.Mass.convert(readDouble(editX), getUnitOfMass(), Units.Mass.G);
	}

	private static int countZeros(double... values)
	{
		int zeros = 0;

		for (double value : values)
		{
			if (value < Utils.EPS)
			{
				zeros++;
			}
		}

		return zeros;
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
						double x = entity.getData().getQ() > Utils.EPS ? entity.getData().getK() / entity.getData().getQ() : 0.0;

						Units.Mass unit = getUnitOfMass();

						switch (index)
						{
							case INDEX_K:
							{
								k = Units.Mass.convert(value, unit, Units.Mass.G);
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
								x = Units.Mass.convert(value, unit, Units.Mass.G);
								editX.setTextColor(getResources().getColor(R.color.font_black));
								break;
							}
						}

						moveToEnd(indexes, index);

						if (countZeros(k, q, x) < 2)
						{
							switch (indexes.iterator().next())
							{
								case INDEX_K:
								{
									k = x * q;
									editK.setText(Utils.formatK(k, unit));
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
									editX.setText(Utils.formatX(x, unit));
									editX.setTextColor(getResources().getColor(R.color.font_gray));
									break;
								}
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
