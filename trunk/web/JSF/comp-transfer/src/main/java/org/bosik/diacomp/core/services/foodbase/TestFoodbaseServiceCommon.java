package org.bosik.diacomp.core.services.foodbase;

import junit.framework.TestCase;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.utils.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.utils.test.fakes.mocks.MockFoodItem;
import org.bosik.diacomp.core.utils.test.fakes.mocks.MockVersionedConverter;
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
		foodBaseService = getService();
	}

	@Override
	public void test_addFindById_single_PersistedOk()
	{
		Versioned<FoodItem> org = mockGenerator.getSamples().get(0);
		String id = org.getId();

		if (foodBaseService.findById(id) == null)
		{
			id = foodBaseService.add(org);
		}

		// ------------------------------
		setUp();
		// ------------------------------

		Versioned<FoodItem> restored = foodBaseService.findById(id);
		assertNotNull(restored);
		assertNotSame(org, restored);
		mockGenerator.compare(org, restored);
	}

	//	@Override
	//	@Test
	//	//(expected = DuplicateException.class)
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
	//(expected = NotFoundException.class)
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

	// TODO: create testPersistenceMultiple()
}
