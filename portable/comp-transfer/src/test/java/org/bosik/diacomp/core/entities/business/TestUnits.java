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
package org.bosik.diacomp.core.entities.business;

import org.bosik.diacomp.core.entities.tech.Coded;
import org.bosik.diacomp.core.utils.CodedUtils;
import org.bosik.diacomp.core.utils.Utils;
import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class TestUnits extends AbstractTestCoded
{
	@Test
	public void test_mass()
	{
		test_all(Units.Mass.class, new HashMap<Units.Mass, String>()
		{
			{
				put(Units.Mass.G, "38be712c8ef74b70b4dba793cc7b349f");
				put(Units.Mass.BU, "56ce855fac9846698a78edf2cacf4cfe");
			}
		});
	}

	@Test
	public void test_bloodsugar()
	{
		test_all(Units.BloodSugar.class, new HashMap<Units.BloodSugar, String>()
		{
			{
				put(Units.BloodSugar.MMOL_L, "ff9e76dd8e144d0d9ab90d15160e9f3b");
				put(Units.BloodSugar.MG_DL, "88101cf56630428aada7fe82388b4b99");
			}
		});
	}
}

abstract class AbstractTestCoded
{
	private static final int EXPECTED_CODE_SIZE = 32;

	/**
	 * Performs complete testing of coded enum
	 *
	 * @param entityClass
	 * @param registeredCodes
	 * @param <T>
	 */
	protected <T extends Enum<T> & Coded> void test_all(Class<T> entityClass, Map<T, String> registeredCodes)
	{
		test_codes(entityClass);
		test_parsing(entityClass, registeredCodes);
		test_parsing_default(entityClass);
	}

	/**
	 * Tests:
	 * <ul>
	 * <li>entityClass is enum</li>
	 * <li>Registered codes have proper size</li>
	 * <li>Number of items equals to number of registered items</li>
	 * <li>Codes are not changed</li>
	 * <li>When parsing unknown code, exception is thrown</li>
	 * </ul>
	 *
	 * @param entityClass
	 * @param registeredCodes
	 * @param <T>
	 */
	protected <T extends Enum<T> & Coded> void test_parsing(Class<T> entityClass, Map<T, String> registeredCodes)
	{
		Assert.assertTrue(entityClass.getName() + " must be enum", entityClass.isEnum());

		for (Map.Entry<T, String> item : registeredCodes.entrySet())
		{
			checkCodeSize(item.getKey(), item.getValue());
		}

		Set<T> enumValues = new HashSet<>(Arrays.asList(entityClass.getEnumConstants()));

		if (enumValues.size() > registeredCodes.size())
		{
			Set<T> difference = Utils.difference(enumValues, registeredCodes.keySet());
			Assert.fail("Add these items to " + getClass().getName() + ": " + difference);
		}

		if (enumValues.size() < registeredCodes.size()) // impossible
		{
			Set<T> difference = Utils.difference(registeredCodes.keySet(), enumValues);
			Assert.fail("Add these items to " + entityClass.getName() + ": " + difference);
		}

		// check if no codes were changed
		for (Map.Entry<T, String> entry : registeredCodes.entrySet())
		{
			Assert.assertEquals(entry.getKey(), CodedUtils.parse(entityClass, entry.getValue()));
		}

		// check if exception is thrown for unknown codes
		try
		{
			CodedUtils.parse(entityClass, "garb3age");
			Assert.fail("IllegalArgumentException must be thrown");
		}
		catch (IllegalArgumentException e)
		{
			// as expected
		}
	}

	/**
	 * Tests:
	 * <ul>
	 * <li>entityClass is enum</li>
	 * <li>Parsing unknown value properly returns default value (including null)</li>
	 * </ul>
	 *
	 * @param entityClass
	 * @param <T>
	 */
	protected <T extends Enum<T> & Coded> void test_parsing_default(Class<T> entityClass)
	{
		Assert.assertTrue(entityClass.getName() + " must be enum", entityClass.isEnum());

		for (T value : new HashSet<>(Arrays.asList(entityClass.getEnumConstants())))
		{
			Assert.assertEquals(value, CodedUtils.parse(entityClass, "garb2age", value));
		}

		Assert.assertEquals(null, CodedUtils.parse(entityClass, "garb2age", null));
	}

	/**
	 * Tests:
	 * <ul>
	 * <li>entityClass is enum</li>
	 * <li>All codes has proper length</li>
	 * <li>Codes have no duplicates</li>
	 * </ul>
	 *
	 * @param entityClass
	 * @param <T>
	 */
	protected <T extends Enum<T> & Coded> void test_codes(Class<T> entityClass)
	{
		Assert.assertTrue(entityClass.getName() + " must be enum", entityClass.isEnum());

		Set<String> codes = new HashSet<>();
		for (T entity : entityClass.getEnumConstants())
		{
			checkCodeSize(entity);
			Assert.assertFalse(entityClass.getName() + " has duplicated code " + entity.getCode(), codes.contains(entity.getCode()));
			codes.add(entity.getCode());
		}
	}

	private <T extends Enum<T> & Coded> void checkCodeSize(T entity)
	{
		checkCodeSize(entity, entity.getCode());
	}

	private <T extends Enum<T> & Coded> void checkCodeSize(T entity, String code)
	{
		Assert.assertNotNull(code);
		Assert.assertEquals(entity.getClass().getName() + "." + entity + " code (" + code + ") has invalid size", EXPECTED_CODE_SIZE,
				code.length());
	}
}
