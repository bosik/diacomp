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

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.BaseAdapter;
import android.widget.ListView;
import android.widget.TextView;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.backend.features.analyze.KoofServiceInternal;
import org.bosik.diacomp.android.backend.features.preferences.account.PreferencesLocalService;
import org.bosik.diacomp.android.frontend.UIUtils;
import org.bosik.diacomp.android.utils.ErrorHandler;
import org.bosik.diacomp.core.entities.business.Rate;
import org.bosik.diacomp.core.services.analyze.KoofService;
import org.bosik.diacomp.core.services.analyze.entities.Koof;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.services.preferences.PreferencesTypedService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.merklesync.HashUtils;
import org.bosik.merklesync.Versioned;
import org.json.JSONException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public class ActivityRates extends Activity
{
	// Constants
	private static final String TAG                = ActivityRates.class.getSimpleName();
	private static final int    DIALOG_RATE_CREATE = 11;
	private static final int    DIALOG_RATE_MODIFY = 12;

	// components
	private ListView    list;
	private BaseAdapter adapter;

	// data
	private List<Versioned<Rate>> rates; // TODO: save/restore on activity re-creation
	private boolean BU = true;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_rates);

		PreferencesTypedService preferences = new PreferencesTypedService(new PreferencesLocalService(this));
		String data = preferences.getStringValue(PreferenceID.RATES_DATA);
		rates = readRatesSafely(data);
		//rates = copyRealRates();

		list = (ListView) findViewById(R.id.listRates);

		list.setOnItemClickListener(new AdapterView.OnItemClickListener()
		{
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long itemIndex)
			{
				showRateEditor(rates.get(position), false);
			}
		});

		adapter = new BaseAdapter()
		{
			@Override
			public int getCount()
			{
				return rates.size();
			}

			@Override
			public Object getItem(int position)
			{
				if (position >= 0 && position < rates.size())
				{
					return rates.get(position);
				}
				else
				{
					return null;
				}
			}

			@Override
			public boolean hasStableIds()
			{
				return true;
			}

			@Override
			public long getItemId(int position)
			{
				Object item = getItem(position);
				if (item != null)
				{
					return ((Versioned<Rate>) item).getData().getTime();
				}
				else
				{
					return position;
				}
			}

			@Override
			public View getView(int position, View view, ViewGroup parent)
			{
				Versioned<Rate> rate = (Versioned<Rate>) getItem(position);

				if (rate != null)
				{
					if (view == null)
					{
						view = getLayoutInflater().inflate(R.layout.view_rate, null);
					}

					((TextView) view.findViewById(R.id.ratesItemTime)).setText(Utils.formatTimeMin(rate.getData().getTime()));

					((TextView) view.findViewById(R.id.ratesItemK)).setText(formatK(rate.getData(), BU));
					((TextView) view.findViewById(R.id.ratesItemKUnit)).setText(formatKUnit(BU));

					((TextView) view.findViewById(R.id.ratesItemQ)).setText(formatQ(rate.getData()));
					((TextView) view.findViewById(R.id.ratesItemQUnit)).setText(formatQUnit());

					((TextView) view.findViewById(R.id.ratesItemX)).setText(formatX(rate.getData(), BU));
					((TextView) view.findViewById(R.id.ratesItemXUnit)).setText(formatXUnit(BU));
				}

				view.setBackgroundDrawable(getResources().getDrawable(R.drawable.background_base_item));
				return view;
			}
		};

		list.setAdapter(adapter);
	}

	// handled
	private void showRateEditor(Versioned<Rate> entity, boolean createMode)
	{
		try
		{
			if (createMode)
			{
				Rate rec = new Rate();
				rec.setTime(0); // FIXME
				entity = new Versioned<>(rec);
			}

			Intent intent = new Intent(this, ActivityEditorRate.class);
			intent.putExtra(ActivityEditor.FIELD_ENTITY, entity);
			intent.putExtra(ActivityEditor.FIELD_CREATE_MODE, createMode);
			intent.putExtra(ActivityEditorRate.KEY_INTENT_USE_BU, BU);

			startActivityForResult(intent, createMode ? DIALOG_RATE_CREATE : DIALOG_RATE_MODIFY);
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
	}

	private String formatK(Rate rate, boolean BU)
	{
		return "K: " + Utils.formatK(rate.getK(), BU);
	}

	private String formatKUnit(boolean BU)
	{
		String unitBS = getString(R.string.common_unit_bs_mmoll);
		// TODO: i18n
		String unitMass = BU ? "ХЕ" : getString(R.string.common_unit_mass_gramm);
		return unitBS + "/" + unitMass;
	}

	private String formatQ(Rate rate)
	{
		return "Q: " + Utils.formatQ(rate.getQ());
	}

	private String formatQUnit()
	{
		String unitBS = getString(R.string.common_unit_bs_mmoll);
		String unitDosage = getString(R.string.common_unit_insulin);
		return unitBS + "/" + unitDosage;
	}

	private String formatX(Rate rate, boolean BU)
	{
		return "X: " + Utils.formatX(rate.getK() / rate.getQ(), BU);
	}

	private String formatXUnit(boolean BU)
	{
		String unitDosage = getString(R.string.common_unit_insulin);
		// TODO: i18n
		String unitMass = BU ? "ХЕ" : getString(R.string.common_unit_mass_gramm);
		return unitDosage + "/" + unitMass;
	}

	private List<Rate> copyRealRates()
	{
		List<Rate> rates = new ArrayList<>();

		KoofService service = KoofServiceInternal.getInstance(this);
		for (int time = 0; time < 1440; time += 120)
		{
			Koof c = service.getKoof(time);

			Rate rate = new Rate();
			rate.setTime(time);
			rate.setK(c.getK());
			rate.setQ(c.getQ());
			rate.setP(c.getP());

			rates.add(rate);
		}

		return rates;
	}

	private List<Versioned<Rate>> readRatesSafely(String data)
	{
		try
		{
			List<Versioned<Rate>> versioned = new ArrayList<>();
			for (Rate rate : Rate.readList(data))
			{
				Versioned<Rate> item = new Versioned<>(rate);
				item.setId(HashUtils.generateGuid());
				versioned.add(item);
			}
			return versioned;
		}
		catch (JSONException e)
		{
			Log.e(TAG, "Failed to read rates JSON: " + data, e);
			return new ArrayList<>();
		}
	}

	@Override
	public void onActivityResult(int requestCode, int resultCode, Intent intent)
	{
		super.onActivityResult(requestCode, resultCode, intent);

		try
		{
			switch (requestCode)
			{
				case DIALOG_RATE_CREATE:
				{
					if (resultCode == Activity.RESULT_OK)
					{
						Versioned<Rate> rec = (Versioned<Rate>) intent.getExtras().getSerializable(ActivityEditor.FIELD_ENTITY);
						rates.add(rec);

						sortByTime(rates);
						save(rates);
						adapter.notifyDataSetChanged();
					}
					break;
				}

				case DIALOG_RATE_MODIFY:
				{
					if (resultCode == Activity.RESULT_OK)
					{
						Versioned<Rate> rec = (Versioned<Rate>) intent.getExtras().getSerializable(ActivityEditor.FIELD_ENTITY);

						rates.remove(rec);
						rates.add(rec);

						sortByTime(rates);
						save(rates);
						adapter.notifyDataSetChanged();
					}
					break;
				}
			}
		}
		catch (Exception e)
		{
			ErrorHandler.handle(e, this);
		}
	}

	private void save(List<Versioned<Rate>> versioned)
	{
		List<Rate> rates = new ArrayList<>();
		for (Versioned<Rate> item : versioned)
		{
			rates.add(item.getData());
		}

		try
		{
			PreferencesTypedService preferences = new PreferencesTypedService(new PreferencesLocalService(this));
			String json = Rate.writeList(rates);
			preferences.setStringValue(PreferenceID.RATES_DATA, json);
		}
		catch (JSONException e)
		{
			UIUtils.showTip(this, "Failed to save rates"); // TODO: i18n
			e.printStackTrace();
		}
	}

	private void sortByTime(List<Versioned<Rate>> rates)
	{
		Collections.sort(rates, new Comparator<Versioned<Rate>>()
		{
			@Override
			public int compare(Versioned<Rate> lhs, Versioned<Rate> rhs)
			{
				return lhs.getData().getTime() - rhs.getData().getTime();
			}
		});
	}
}
