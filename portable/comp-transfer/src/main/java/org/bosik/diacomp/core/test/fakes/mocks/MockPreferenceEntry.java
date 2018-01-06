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
package org.bosik.diacomp.core.test.fakes.mocks;

import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.merklesync.HashUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNotNull;

public class MockPreferenceEntry implements Mock<PreferenceEntry<String>>
{
	private final Random r = new Random();

	@Override
	public List<PreferenceEntry<String>> getSamples()
	{
		List<PreferenceEntry<String>> samples = new ArrayList<>();

		for (int i = 0; i < 10; i++)
		{
			samples.add(getSample());
		}

		return samples;
	}

	@Override
	public PreferenceEntry<String> getSample()
	{
		PreferenceEntry<String> entry = new PreferenceEntry<>();

		entry.setId(PreferenceID.values()[r.nextInt(PreferenceID.values().length)]);
		entry.setValue(HashUtils.generateGuid() + "\"}{'\t\n\r\b\f><\\&");
		entry.setVersion(r.nextInt());

		return entry;
	}

	@Override
	public void compare(PreferenceEntry<String> exp, PreferenceEntry<String> act)
	{
		assertNotNull(exp);
		assertNotNull(act);

		assertEquals(exp.getId(), act.getId());
		assertEquals(exp.getValue(), act.getValue());
		assertEquals(exp.getVersion(), act.getVersion());
	}
}
