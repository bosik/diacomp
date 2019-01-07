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
package org.bosik.diacomp.core.utils;

import org.bosik.diacomp.core.entities.tech.Coded;
import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public abstract class AbstractTestCoded<T extends Enum<T> & Coded>
{
	private static final int EXPECTED_CODE_SIZE = 32;

	@Test
	public void test()
	{
		test_all(getEntityClass(), getPublishedCodes());
	}

	protected abstract Class<T> getEntityClass();

	protected abstract Map<T, String> getPublishedCodes();

	/**
	 * Performs complete testing of coded enum
	 *
	 * @param entityClass
	 * @param registeredCodes
	 */
	protected void test_all(Class<T> entityClass, Map<T, String> registeredCodes)
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
	 */
	protected void test_parsing(Class<T> entityClass, Map<T, String> registeredCodes)
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
	 */
	protected void test_parsing_default(Class<T> entityClass)
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
	 */
	protected void test_codes(Class<T> entityClass)
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

	private void checkCodeSize(T entity)
	{
		checkCodeSize(entity, entity.getCode());
	}

	private void checkCodeSize(T entity, String code)
	{
		Assert.assertNotNull(code);
		Assert.assertEquals(entity.getClass().getName() + "." + entity + " code (" + code + ") has invalid size", EXPECTED_CODE_SIZE,
				code.length());
	}
}
