/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.core.services.preferences;

import org.bosik.diacomp.core.utils.AbstractTestCoded;
import org.junit.Assert;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

public class TestPreferenceID extends AbstractTestCoded<PreferenceID>
{
	@Override
	protected Class<PreferenceID> getEntityClass()
	{
		return PreferenceID.class;
	}

	@Override
	protected HashMap<PreferenceID, String> getPublishedCodes()
	{
		return new HashMap<PreferenceID, String>()
		{
			{
				add("e6681282aa724d3fa4cd6ac5735a163f", PreferenceID.TARGET_BS);
				add("1a25c92eaa3148219da83b1e66275052", PreferenceID.FOOD_SETS);
				add("e92b955f48fa434d960fdc4a541490de", PreferenceID.RATES_AUTO);
				add("7648a35d3fbe4b4ca8fab12876abb1b6", PreferenceID.RATES_DATA);
				add("589985b443c243d6844209964b2b1e8e", PreferenceID.RATES_MASS_UNITS);
				add("8b6575e476d64becae68468500f1bc1c", PreferenceID.ANDROID_FIRST_START);
				add("d5c1a902e83b4d05a51085e344bee953", PreferenceID.ANDROID_SHOW_TIME_AFTER);
				add("f3f54f8f02a3411faf48f90aadf0ca2d", PreferenceID.ANDROID_MEAL_FORMAT);
			}

			private void add(String code, PreferenceID entry)
			{
				put(entry, code);
			}
		};
	}

	@Test
	public void test_types()
	{
		Map<PreferenceID, Type> types = new HashMap<PreferenceID, Type>()
		{
			{
				put(PreferenceID.TARGET_BS, Type.FLOAT);
				put(PreferenceID.FOOD_SETS, Type.STRING);
				put(PreferenceID.RATES_AUTO, Type.BOOLEAN);
				put(PreferenceID.RATES_DATA, Type.STRING);
				put(PreferenceID.RATES_MASS_UNITS, Type.STRING);
				put(PreferenceID.ANDROID_FIRST_START, Type.BOOLEAN);
				put(PreferenceID.ANDROID_SHOW_TIME_AFTER, Type.BOOLEAN);
				put(PreferenceID.ANDROID_MEAL_FORMAT, Type.STRING);
			}
		};

		// check all items are tested
		for (PreferenceID p : PreferenceID.values())
		{
			if (!types.containsKey(p))
			{
				Assert.fail("Add this preference to test: " + p);
			}
		}

		// check types are not changed
		for (Map.Entry<PreferenceID, Type> entry : types.entrySet())
		{
			Assert.assertEquals(entry.getValue(), entry.getKey().getType());
		}
	}
}
