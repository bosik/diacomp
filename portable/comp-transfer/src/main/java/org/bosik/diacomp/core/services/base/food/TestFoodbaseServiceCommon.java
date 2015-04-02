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
package org.bosik.diacomp.core.services.base.food;

import java.util.Date;
import java.util.List;
import junit.framework.TestCase;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.test.fakes.mocks.MockFoodItem;
import org.bosik.diacomp.core.test.fakes.mocks.MockVersionedConverter;
import org.bosik.merklesync.Versioned;
import org.junit.Test;

public abstract class TestFoodbaseServiceCommon extends TestCase implements TestFoodbaseService
{
	private FoodBaseService						foodBaseService;
	private static Mock<Versioned<FoodItem>>	mockGenerator	= new MockVersionedConverter<FoodItem>(
																		new MockFoodItem());

	protected abstract FoodBaseService getService();

	@Override
	protected void setUp()
	{
		init();
	}

	public void init()
	{
		foodBaseService = getService();
		assertNotNull(foodBaseService);
	}

	@Override
	public void test_addFindById_single_PersistedOk()
	{
		Versioned<FoodItem> org = mockGenerator.getSamples().get(0);
		String id = org.getId();

		if (foodBaseService.findById(id) == null)
		{
			foodBaseService.add(org);
		}

		// ------------------------------
		setUp();
		// ------------------------------

		Versioned<FoodItem> restored = foodBaseService.findById(id);
		assertNotNull(restored);
		assertNotSame(org, restored);
		mockGenerator.compare(org, restored);
	}

	@Override
	public void test_addFindChanged_single_ReturnedNonEmpty()
	{
		Versioned<FoodItem> org = mockGenerator.getSamples().get(0);
		org.setTimeStamp(new Date());
		String id = org.getId();

		if (foodBaseService.findById(id) == null)
		{
			foodBaseService.add(org);
		}

		// ------------------------------
		setUp();
		// ------------------------------

		Date since = new Date(org.getTimeStamp().getTime() - 1000);
		List<Versioned<FoodItem>> restored = foodBaseService.findChanged(since);
		assertNotNull(restored);
		assertTrue(!restored.isEmpty());
	}

	//	@Override
	//	@Test
	//	public void test_add_duplication_exceptionRaised()
	//	{
	//		try
	//		{
	//			Versioned<FoodItem> org = mockGenerator.getSamples().get(0);
	//			foodBaseService.add(org);
	//			foodBaseService.add(org);
	//			fail("DuplicateException expected");
	//		}
	//		catch (DuplicateException e)
	//		{
	//			// it's ok, just as planned
	//		}
	//	}

	@Override
	@Test
	public void test_delete_notFound_exceptionRaised()
	{
		try
		{
			Versioned<FoodItem> org = mockGenerator.getSamples().get(0);
			foodBaseService.delete(org.getId());
			foodBaseService.delete(org.getId());
			fail("NotFoundException expected");
		}
		catch (NotFoundException e)
		{
			// it's ok, just as planned
		}
	}

	@Override
	public void test_addFindAll_single_ReturnedNonEmpty()
	{
		Versioned<FoodItem> org = mockGenerator.getSamples().get(0);
		String id = org.getId();

		if (foodBaseService.findById(id) == null)
		{
			foodBaseService.add(org);
		}

		// ------------------------------
		setUp();
		// ------------------------------

		List<Versioned<FoodItem>> restored = foodBaseService.findAll(false);
		assertNotNull(restored);
		assertTrue(!restored.isEmpty());
	}

	@Override
	public void test_addFindAny_single_ok()
	{
		Versioned<FoodItem> org = mockGenerator.getSamples().get(0);
		org.setDeleted(false);
		String id = org.getId();

		if (foodBaseService.findById(id) == null)
		{
			foodBaseService.add(org);
		}

		String filter = org.getData().getName().substring(0, 2).toLowerCase();
		List<Versioned<FoodItem>> restored = foodBaseService.findAny(filter);

		assertNotNull("Returned list is null", restored);
		assertTrue("Returned list is empty", !restored.isEmpty());
		for (Versioned<FoodItem> item : restored)
		{
			final String name = item.getData().getName().toLowerCase();
			assertTrue(name.contains(filter));
		}
	}

	// TODO: create testPersistenceMultiple()
}
