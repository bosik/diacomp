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
package org.bosik.diacomp.core.services.base.dish;

import java.util.Date;
import java.util.List;
import junit.framework.TestCase;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.mocks.Mock;
import org.bosik.diacomp.core.mocks.MockDishItem;
import org.bosik.diacomp.core.mocks.MockVersionedConverter;
import org.bosik.merklesync.Versioned;
import org.junit.Test;

public abstract class TestDishbaseServiceCommon extends TestCase implements TestDishbaseService
{
	private DishBaseService						dishBaseService;
	private static Mock<Versioned<DishItem>>	mockGenerator	= new MockVersionedConverter<>(
			new MockDishItem());

	protected abstract DishBaseService getService();

	@Override
	protected void setUp()
	{
		init();
	}

	public void init()
	{
		dishBaseService = getService();
		assertNotNull(dishBaseService);
	}

	@Override
	public void test_addFindById_single_PersistedOk()
	{
		Versioned<DishItem> org = mockGenerator.getSamples().get(0);
		String id = org.getId();

		if (dishBaseService.findById(id) == null)
		{
			dishBaseService.add(org);
		}

		// ------------------------------
		setUp();
		// ------------------------------

		Versioned<DishItem> restored = dishBaseService.findById(id);
		assertNotNull(restored);
		assertNotSame(org, restored);
		mockGenerator.compare(org, restored);
	}

	@Override
	public void test_addFindChanged_single_ReturnedNonEmpty()
	{
		Versioned<DishItem> org = mockGenerator.getSamples().get(0);
		org.setTimeStamp(new Date());
		String id = org.getId();

		if (dishBaseService.findById(id) == null)
		{
			dishBaseService.add(org);
		}

		// ------------------------------
		setUp();
		// ------------------------------

		Date since = new Date(org.getTimeStamp().getTime() - 1000);
		List<Versioned<DishItem>> restored = dishBaseService.findChanged(since);
		assertNotNull(restored);
		assertTrue(!restored.isEmpty());
	}

	//	@Override
	//	@Test
	//	public void test_add_duplication_exceptionRaised()
	//	{
	//		try
	//		{
	//			Versioned<DishItem> org = mockGenerator.getSamples().get(0);
	//			dishBaseService.add(org);
	//			dishBaseService.add(org);
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
			Versioned<DishItem> org = mockGenerator.getSamples().get(0);
			dishBaseService.delete(org.getId());
			dishBaseService.delete(org.getId());
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
		Versioned<DishItem> org = mockGenerator.getSamples().get(0);
		String id = org.getId();

		if (dishBaseService.findById(id) == null)
		{
			dishBaseService.add(org);
		}

		// ------------------------------
		setUp();
		// ------------------------------

		List<Versioned<DishItem>> restored = dishBaseService.findAll(false);
		assertNotNull(restored);
		assertTrue(!restored.isEmpty());
	}

	@Override
	public void test_addFindAny_single_ok()
	{
		Versioned<DishItem> org = mockGenerator.getSamples().get(0);
		org.setDeleted(false);
		String id = org.getId();

		if (dishBaseService.findById(id) == null)
		{
			dishBaseService.add(org);
		}

		String filter = org.getData().getName().substring(0, 2).toLowerCase();
		List<Versioned<DishItem>> restored = dishBaseService.findAny(filter);

		assertNotNull("Returned list is null", restored);
		assertTrue("Returned list is empty", !restored.isEmpty());
		for (Versioned<DishItem> item : restored)
		{
			final String name = item.getData().getName().toLowerCase();
			assertTrue(name.contains(filter));
		}
	}

	// TODO: create testPersistenceMultiple()
}
